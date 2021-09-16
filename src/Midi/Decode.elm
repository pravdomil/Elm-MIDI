module Midi.Decode exposing (normalise, file, event)

{-| Module for decoding MIDI.

@docs normalise, file, event

-}

import Parser exposing (Parser)


midi : Parser MidiRecording
midi =
    midiHeader
        |> andThen midiTracks



{- an internal representation of the header which includes the track count -}


type alias Header =
    { formatType : Int
    , trackCount : Int
    , ticksPerBeat : Int
    }



{- parser for headers which quietly eats any extra bytes if we have a non-standard chunk size -}


midiHeader : Parser Header
midiHeader =
    string "MThd"
        *> (let
                h =
                    headerChunk <$> uInt32 <*> uInt16 <*> uInt16 <*> uInt16
            in
            consumeOverspill h 6
                <?> "header"
           )


midiTracks : Header -> Parser MidiRecording
midiTracks h =
    case h.formatType of
        0 ->
            if h.trackCount == 1 then
                SingleTrack h.ticksPerBeat <$> midiTrack <?> "midi track for single track file"

            else
                fail ("Single track file with " ++ toString h.trackCount ++ " tracks.")

        1 ->
            MultipleTracks Simultaneous h.ticksPerBeat
                <$> (count h.trackCount midiTrack <?> "midi track for simultaneous tracks file")

        2 ->
            MultipleTracks Independent h.ticksPerBeat
                <$> (count h.trackCount midiTrack <?> "midi track for independent tracks file")

        f ->
            fail ("Unknown MIDI file format " ++ toString f)



{- we pass Nothing to midiMessages because the very first message
   has no parent (antecedent)
-}


midiTrack : Parser Track
midiTrack =
    string "MTrk" *> uInt32 *> midiMessages Nothing <?> "midi track"


midiMessages : Maybe MidiEvent -> Parser (List MidiMessage)
midiMessages parent =
    midiMessage parent
        >>= continueOrNot



{- Keep reading unless we just saw an End of Track message
   which would cause us to get Nothing passed in here
-}


continueOrNot : ( Ticks, Maybe MidiEvent ) -> Parser (List MidiMessage)
continueOrNot maybeLastMessage =
    case maybeLastMessage of
        ( ticks, Just lastEvent ) ->
            (::) ( ticks, lastEvent )
                <$> midiMessages (Just lastEvent)

        ( _, Nothing ) ->
            succeed []


midiMessage : Maybe MidiEvent -> Parser ( Ticks, Maybe MidiEvent )
midiMessage parent =
    (,)
        <$> varInt
        <*> midiFileEvent parent
        <?> "midi message"



{- we need to pass the parent event to running status events in order to
   make sense of them.
-}


midiEvent : Maybe MidiEvent -> Parser MidiEvent
midiEvent parent =
    choice
        [ metaEvent
        , sysExEvent
        , noteOn
        , noteOff
        , noteAfterTouch
        , controlChange
        , programChange
        , channelAfterTouch
        , pitchBend
        ]
        <?> "midi event"


midiFileEvent : Maybe MidiEvent -> Parser (Maybe MidiEvent)
midiFileEvent parent =
    choice
        [ metaFileEvent
        , Just <$> noteOn
        , Just <$> noteOff
        , Just <$> noteAfterTouch
        , Just <$> controlChange
        , Just <$> programChange
        , Just <$> channelAfterTouch
        , Just <$> pitchBend
        , Just <$> fileSysExEvent
        , Just <$> runningStatus parent
        ]
        <?> "midi event"



-- metadata parsers


metaEvent : Parser MidiEvent
metaEvent =
    bChar 0xFF
        *> choice
            [ parseSequenceNumber
            , parseText
            , parseCopyright
            , parseTrackName
            , parseInstrumentName
            , parseLyrics
            , parseMarker
            , parseCuePoint
            , parseChannelPrefix
            , parseTempoChange
            , parseSMPTEOffset
            , parseTimeSignature
            , parseKeySignature
            , parseSequencerSpecific
            , parseUnspecified
            ]
        <?> "meta event"


metaFileEvent : Parser (Maybe MidiEvent)
metaFileEvent =
    bChar 0xFF
        *> choice
            [ Just <$> parseSequenceNumber
            , Just <$> parseText
            , Just <$> parseCopyright
            , Just <$> parseTrackName
            , Just <$> parseInstrumentName
            , Just <$> parseLyrics
            , Just <$> parseMarker
            , Just <$> parseCuePoint
            , Just <$> parseChannelPrefix
            , Just <$> parseTempoChange
            , Just <$> parseSMPTEOffset
            , Just <$> parseTimeSignature
            , Just <$> parseKeySignature
            , Just <$> parseSequencerSpecific
            , parseEndOfTrack
            , Just <$> parseUnspecified
            ]
        <?> "meta event"


parseEndOfTrack : Parser (Maybe MidiEvent)
parseEndOfTrack =
    bChar 0x2F *> bChar 0x00 *> succeed Nothing <?> "sequence number"


parseSequenceNumber : Parser MidiEvent
parseSequenceNumber =
    SequenceNumber <$> (bChar 0x00 *> bChar 0x02 *> uInt16 <?> "sequence number")



{- parse a simple string-valued meta event -}


parseMetaString : Int -> Parser String
parseMetaString target =
    String.fromList
        -- <$> (bchar target *> varInt `andThen` (\l -> count l anyChar))
        <$> (bChar target *> varInt >>= (\l -> count l anyChar))



{- parse a meta event valued as a List of Bytes (masquerading as Ints) -}


parseMetaBytes : Int -> Parser (List Byte)
parseMetaBytes target =
    List.map toCode
        <$> (bChar target *> varInt >>= (\l -> count l anyChar))


parseText : Parser MidiEvent
parseText =
    Text <$> parseMetaString 0x01 <?> "text"



-- parseText = log "text" <$> (Text <$> parseMetaString 0x01 <?> "text" )


parseCopyright : Parser MidiEvent
parseCopyright =
    Copyright <$> parseMetaString 0x02 <?> "copyright"


parseTrackName : Parser MidiEvent
parseTrackName =
    TrackName <$> parseMetaString 0x03 <?> "track name"



{-
   parseTrackName =
       log "track name" <$> (TrackName <$> parseMetaString 0x03 <?> "track name")
-}


parseInstrumentName : Parser MidiEvent
parseInstrumentName =
    InstrumentName <$> parseMetaString 0x04 <?> "instrument name"


parseLyrics : Parser MidiEvent
parseLyrics =
    Lyrics <$> parseMetaString 0x05 <?> "lyrics"


parseMarker : Parser MidiEvent
parseMarker =
    Marker <$> parseMetaString 0x06 <?> "marker"


parseCuePoint : Parser MidiEvent
parseCuePoint =
    CuePoint <$> parseMetaString 0x07 <?> "cue point"


parseChannelPrefix : Parser MidiEvent
parseChannelPrefix =
    ChannelPrefix <$> (bChar 0x20 *> bChar 0x01 *> uInt8 <?> "channel prefix")


parseTempoChange : Parser MidiEvent
parseTempoChange =
    Tempo <$> (bChar 0x51 *> bChar 0x03 *> uInt24) <?> "tempo change"


parseSMPTEOffset : Parser MidiEvent
parseSMPTEOffset =
    bChar 0x54 *> bChar 0x03 *> (SMPTEOffset <$> uInt8 <*> uInt8 <*> uInt8 <*> uInt8 <*> uInt8 <?> "SMTPE offset")


parseTimeSignature : Parser MidiEvent
parseTimeSignature =
    bChar 0x58 *> bChar 0x04 *> (buildTimeSig <$> uInt8 <*> uInt8 <*> uInt8 <*> uInt8) <?> "time signature"


parseKeySignature : Parser MidiEvent
parseKeySignature =
    bChar 0x59 *> bChar 0x02 *> (KeySignature <$> signedInt8 <*> uInt8)


parseSequencerSpecific : Parser MidiEvent
parseSequencerSpecific =
    SequencerSpecific <$> parseMetaBytes 0x7F <?> "sequencer specific"



{- A SysEx event is introduced by an 0xF0 byte and is followed by an array of bytes.
   In Web Midi a sysex event starts with a 0xF0 byte and ends with an EOX (0xF7) byte.
   There are also escaped SysEx messages, but these are only found in MIDI files.
-}


sysExEvent : Parser MidiEvent
sysExEvent =
    let
        eoxChar =
            fromCode eox
    in
    (\bytes -> SysEx F0 bytes)
        <$> (List.map toCode
                <$> (String.toList
                        <$> (bChar 0xF0 *> while ((/=) eoxChar))
                    )
            )
        <?> "system exclusive"



{- A SysEx event in a file is introduced by an 0xF0 or 0xF7 byte, followed by a
   variable length integer that denotes how many data bytes follow.
   If it starts with 0xF0 the data bytes must be valid sysex data, however if it
   starts with 0xF7 any data may follow.
   Note: Since this library doesn't do anything special to handle multi-part
   SysEx messages it must record the EOX byte as part of the SysEx message
   here as opposed to for SysEx MIDI events where that byte can be left
   implicit.
-}


fileSysExEvent : Parser MidiEvent
fileSysExEvent =
    let
        parseFlavour : Parser SysExFlavour
        parseFlavour =
            (bChar 0xF0 $> F0) <|> (bChar 0xF7 $> F7) <?> "sysex flavour"

        sysexData : Parser Char
        sysexData =
            satisfy (\c -> toCode c < 128)

        parseUnescapedSysex : Parser MidiEvent
        parseUnescapedSysex =
            SysEx
                <$> (bChar 0xF0 $> F0)
                <*> (List.map toCode
                        <$> (varInt
                                >>= (\n -> count n sysexData)
                            )
                    )
                <?> "unescaped system exclusive"

        parseEscapedSysex : Parser MidiEvent
        parseEscapedSysex =
            SysEx
                <$> (bChar 0xF7 $> F7)
                <*> (List.map toCode
                        <$> (varInt
                                >>= (\n -> count n anyChar)
                            )
                    )
                <?> "escaped system exclusive"
    in
    (parseUnescapedSysex <|> parseEscapedSysex)
        <?> "system exclusive (MIDI file)"



{- parse an unspecified meta event
   The possible range for the type is 00-7F. Not all values in this range are defined, but programs must be able
   to cope with (ie ignore) unexpected values by examining the length and skipping over the data portion.
   We cope by accepting any value here except TrackEnd which is the terminating condition for the list of MidiEvents
   and so must not be recognized here
-}


parseUnspecified : Parser MidiEvent
parseUnspecified =
    Unspecified <$> notTrackEnd <*> (uInt8 >>= (\l -> count l uInt8))



{- parse an entire Track End message - not simply the event -}


trackEndMessage : Parser ()
trackEndMessage =
    varInt *> bChar 0xFF *> bChar 0x2F *> bChar 0x00 *> succeed () <?> "track end"



-- trackEndMessage =   log "track end" <$>  (varInt *> bchar 0xFF *> bchar 0x2F *> bchar 0x00 *> succeed () <?> "track end" )
-- channel parsers


noteOn : Parser MidiEvent
noteOn =
    buildNote <$> bRange 0x90 0x9F <*> uInt8 <*> uInt8 <?> "note on"



-- noteOn = log "note on" <$> ( buildNote <$> brange 0x90 0x9F <*> int8 <*> int8 <?> "note on" )


noteOff : Parser MidiEvent
noteOff =
    buildNoteOff <$> bRange 0x80 0x8F <*> uInt8 <*> uInt8 <?> "note off"



-- noteOff = log "note off" <$> ( buildNoteOff <$> brange 0x80 0x8F <*> int8 <*> int8 <?> "note off" )


noteAfterTouch : Parser MidiEvent
noteAfterTouch =
    buildNoteAfterTouch <$> bRange 0xA0 0xAF <*> uInt8 <*> uInt8 <?> "note after touch"



-- noteAfterTouch = log "note afterTouch" <$> ( buildNoteAfterTouch <$> brange 0xA0 0xAF <*> int8 <*> int8 <?> "note after touch" )


controlChange : Parser MidiEvent
controlChange =
    buildControlChange <$> bRange 0xB0 0xBF <*> uInt8 <*> uInt8 <?> "control change"



-- controlChange = log "control change" <$> ( buildControlChange <$> brange 0xB0 0xBF <*> int8 <*> int8 <?> "control change" )


programChange : Parser MidiEvent
programChange =
    buildProgramChange <$> bRange 0xC0 0xCF <*> uInt8 <?> "program change"



-- programChange = log "program change" <$> ( buildProgramChange <$> brange 0xC0 0xCF <*> int8 <?> "program change" )


channelAfterTouch : Parser MidiEvent
channelAfterTouch =
    buildChannelAfterTouch <$> bRange 0xD0 0xDF <*> uInt8 <?> "channel after touch"



-- channelAfterTouch = log "channel afterTouch" <$> ( buildChannelAfterTouch <$> brange 0xD0 0xDF <*> int8 <?> "channel after touch")


pitchBend : Parser MidiEvent
pitchBend =
    buildPitchBend <$> bRange 0xE0 0xEF <*> uInt8 <*> uInt8 <?> "pitch bend"



{- running status is somewhat anomalous.  It inherits the 'type' of the last event parsed,
   (here called the parent) which must be a channel event.
   We now macro-expand the running status message to be the type (and use the channel status)
   of the parent.  If the parent is missing or is not a channel event, we fail the parse
-}


runningStatus : Maybe MidiEvent -> Parser MidiEvent
runningStatus parent =
    case parent of
        Just (NoteOn status _ _) ->
            NoteOn status <$> uInt8 <*> uInt8 <?> "note on running status"

        Just (NoteOff status _ _) ->
            NoteOff status <$> uInt8 <*> uInt8 <?> "note off running status"

        Just (NoteAfterTouch status _ _) ->
            NoteAfterTouch status <$> uInt8 <*> uInt8 <?> "note aftertouch running status"

        Just (ControlChange status _ _) ->
            ControlChange status <$> uInt8 <*> uInt8 <?> "control change running status"

        Just (ProgramChange status _) ->
            ProgramChange status <$> uInt8 <?> "program change running status"

        Just (ChannelAfterTouch status _) ->
            ChannelAfterTouch status <$> uInt8 <?> "channel aftertouch running status"

        Just (PitchBend status _) ->
            PitchBend status <$> uInt8 <?> "pitch bend running status"

        Just _ ->
            fail "inappropriate parent for running status"

        _ ->
            fail "no parent for running status"


headerChunk : Int -> Int -> Int -> Int -> ( Int, Header )
headerChunk l a b c =
    ( l, Header a b c )



{- build NoteOn (unless the velocity is zero in which case NoteOff) -}


buildNote : Int -> Int -> Int -> MidiEvent
buildNote cmd note velocity =
    let
        channel =
            -- cmd `and` 0x0F
            and cmd 0x0F

        isOff =
            velocity == 0
    in
    case isOff of
        True ->
            NoteOff channel note velocity

        _ ->
            NoteOn channel note velocity



{- abstract builders that construct MidiEvents that all have the same shape -}


channelBuilder3 : (Int -> Int -> Int -> MidiEvent) -> Int -> Int -> Int -> MidiEvent
channelBuilder3 construct cmd x y =
    let
        channel =
            -- cmd `and` 0x0F
            and cmd 0x0F
    in
    construct channel x y


channelBuilder2 : (Int -> Int -> MidiEvent) -> Int -> Int -> MidiEvent
channelBuilder2 construct cmd x =
    let
        channel =
            -- cmd `and` 0x0F
            and cmd 0x0F
    in
    construct channel x



{- build NoteOff -}


buildNoteOff : Int -> Int -> Int -> MidiEvent
buildNoteOff cmd note velocity =
    channelBuilder3 NoteOff cmd note velocity



{- build Note AfterTouch AKA Polyphonic Key Pressure -}


buildNoteAfterTouch : Int -> Int -> Int -> MidiEvent
buildNoteAfterTouch cmd note pressure =
    channelBuilder3 NoteAfterTouch cmd note pressure



{- build Control Change -}


buildControlChange : Int -> Int -> Int -> MidiEvent
buildControlChange cmd num value =
    channelBuilder3 ControlChange cmd num value



{- build Program Change -}


buildProgramChange : Int -> Int -> MidiEvent
buildProgramChange cmd num =
    channelBuilder2 ProgramChange
        cmd
        num



{- build Channel AfterTouch AKA Channel Key Pressure -}


buildChannelAfterTouch : Int -> Int -> MidiEvent
buildChannelAfterTouch cmd num =
    channelBuilder2 ChannelAfterTouch cmd num



{- build Pitch Bend -}


buildPitchBend : Int -> Int -> Int -> MidiEvent
buildPitchBend cmd lsb msb =
    channelBuilder2 PitchBend cmd <| lsb + shiftLeftBy 7 msb



{- build a Time Signature -}


buildTimeSig : Int -> Int -> Int -> Int -> MidiEvent
buildTimeSig nn dd cc bb =
    let
        denom =
            2 ^ dd
    in
    TimeSignature nn denom cc bb



-- utility functions
{- consume the overspill from a non-standard size chunk
   actual is the parsed actual chunk size followed by the chunk contents (which are returned)
   expected is the expected size of the chunk
   consume the rest if the difference suggests an overspill of unwanted chunk material
-}


consumeOverspill : Parser ( Int, a ) -> Int -> Parser a
consumeOverspill actual expected =
    actual
        >>= (\( cnt, rest ) ->
                map (\_ -> rest) <|
                    skip <|
                        count (cnt - expected) uInt8
            )


makeTuple : a -> b -> ( a, b )
makeTuple a b =
    ( a, b )



-- exported functions


{-| Parse a MIDI event
-}
event : String -> Result String MidiEvent
event s =
    case Combine.parse (midiEvent Nothing) s of
        Ok ( _, _, n ) ->
            Ok n

        Err ( _, ctx, ms ) ->
            Err ("parse error: " ++ toString ms ++ ", " ++ toString ctx)


{-| entry point - Parse a normalised MIDI file image
-}
file : String -> Result String MidiRecording
file s =
    case Combine.parse midi s of
        Ok ( _, _, n ) ->
            Ok n

        Err ( _, ctx, ms ) ->
            Err ("parse error: " ++ toString ms ++ ", " ++ toString ctx)


{-| Normalise the input before we parse by masking off all but the least
significant 8 bits. We assume the string contains only bytes so this
should be a no-op.
-}
normalise : String -> String
normalise =
    let
        f =
            toCode >> and 0xFF >> fromCode
    in
    String.toList >> List.map f >> String.fromList



-- Helpers


varInt : Parser Int
varInt =
    let
        helper : Parser (List Int)
        helper =
            uInt8
                >>= (\n ->
                        if n < 128 then
                            succeed [ n ]

                        else
                            (::) (and 127 n) <$> helper
                    )
    in
    List.foldl (\n -> \acc -> shiftLeftBy 7 acc + n) 0 <$> helper


{-| Parse an 8 bit integer lying within a range.
-}
bRange : Int -> Int -> Parser Int
bRange l r =
    let
        f a =
            toCode a >= l && toCode a <= r
    in
    toCode <$> satisfy f


notTrackEnd : Parser Int
notTrackEnd =
    let
        c =
            fromCode 0x2F
    in
    toCode <$> noneOf [ c ]
