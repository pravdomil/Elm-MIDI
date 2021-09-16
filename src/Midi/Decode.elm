module Midi.Decode exposing (file, event)

{-| Module for decoding MIDI.

@docs file, event

-}

import Bytes exposing (Bytes)
import Bytes.Decode as Decode exposing (Decoder)
import Midi


{-| Decode MIDI file.
-}
file : Bytes -> Maybe Midi.File
file a =
    Decode.decode fileDecoder a


{-| Parse a MIDI event.
-}
event : String -> Result String Midi.Event
event s =
    case Combine.parse (midiEvent Nothing) s of
        Ok ( _, _, n ) ->
            Ok n

        Err ( _, ctx, ms ) ->
            Err ("parse error: " ++ toString ms ++ ", " ++ toString ctx)



--


fileDecoder : Decoder Midi.File
fileDecoder =
    midiHeader
        |> andThen midiTracks


{-| An internal representation of the header which includes the track count.
-}
type alias Header =
    { formatType : Int
    , trackCount : Int
    , ticksPerBeat : Int
    }


{-| Parser for headers which quietly eats any extra bytes if we have a non-standard chunk size.
-}
midiHeader : Decoder Header
midiHeader =
    string "MThd"
        *> (let
                h =
                    headerChunk <$> uInt32 <*> uInt16 <*> uInt16 <*> uInt16
            in
            consumeOverspill h 6
                <?> "header"
           )


midiTracks : Header -> Decoder MidiRecording
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


{-| We pass Nothing to midiMessages because the very first message
has no parent (antecedent).
-}
midiTrack : Decoder Track
midiTrack =
    string "MTrk" *> uInt32 *> midiMessages Nothing <?> "midi track"


midiMessages : Maybe Midi.Event -> Decoder (List MidiMessage)
midiMessages parent =
    midiMessage parent
        >>= continueOrNot


{-| Keep reading unless we just saw an End of Track message
which would cause us to get Nothing passed in here.
-}
continueOrNot : ( Ticks, Maybe Midi.Event ) -> Decoder (List MidiMessage)
continueOrNot maybeLastMessage =
    case maybeLastMessage of
        ( ticks, Just lastEvent ) ->
            (::) ( ticks, lastEvent )
                <$> midiMessages (Just lastEvent)

        ( _, Nothing ) ->
            succeed []


midiMessage : Maybe Midi.Event -> Decoder ( Ticks, Maybe Midi.Event )
midiMessage parent =
    (,)
        <$> varInt
        <*> midiFileEvent parent
        <?> "midi message"


{-| We need to pass the parent event to running status events in order to
make sense of them.
-}
midiEvent : Maybe Midi.Event -> Decoder Midi.Event
midiEvent parent =
    choice
        [ eventMeta
        , eventSysEx
        , eventNoteOn
        , eventNoteOff
        , eventNoteAfterTouch
        , eventControlChange
        , eventProgramChange
        , eventChannelAfterTouch
        , eventPitchBend
        ]
        <?> "midi event"


midiFileEvent : Maybe Midi.Event -> Decoder (Maybe Midi.Event)
midiFileEvent parent =
    choice
        [ eventMetaFile
        , Just <$> eventNoteOn
        , Just <$> eventNoteOff
        , Just <$> eventNoteAfterTouch
        , Just <$> eventControlChange
        , Just <$> eventProgramChange
        , Just <$> eventChannelAfterTouch
        , Just <$> eventPitchBend
        , Just <$> eventFileSysExEvent
        , Just <$> eventRunningStatus parent
        ]
        <?> "midi event"



-- Metadata


eventMeta : Decoder Midi.Event
eventMeta =
    bChar 0xFF
        *> choice
            [ eventSequenceNumber
            , eventText
            , eventCopyright
            , eventTrackName
            , eventInstrumentName
            , eventLyrics
            , eventMarker
            , eventCuePoint
            , eventChannelPrefix
            , eventTempoChange
            , eventSMPTEOffset
            , eventTimeSignature
            , eventKeySignature
            , eventSequencerSpecific
            , eventUnspecified
            ]
        <?> "meta event"


eventMetaFile : Decoder (Maybe Midi.Event)
eventMetaFile =
    bChar 0xFF
        *> choice
            [ Just <$> eventSequenceNumber
            , Just <$> eventText
            , Just <$> eventCopyright
            , Just <$> eventTrackName
            , Just <$> eventInstrumentName
            , Just <$> eventLyrics
            , Just <$> eventMarker
            , Just <$> eventCuePoint
            , Just <$> eventChannelPrefix
            , Just <$> eventTempoChange
            , Just <$> eventSMPTEOffset
            , Just <$> eventTimeSignature
            , Just <$> eventKeySignature
            , Just <$> eventSequencerSpecific
            , parseEndOfTrack
            , Just <$> eventUnspecified
            ]
        <?> "meta event"


parseEndOfTrack : Decoder (Maybe Midi.Event)
parseEndOfTrack =
    bChar 0x2F *> bChar 0x00 *> succeed Nothing <?> "sequence number"


eventSequenceNumber : Decoder Midi.Event
eventSequenceNumber =
    SequenceNumber <$> (bChar 0x00 *> bChar 0x02 *> uInt16 <?> "sequence number")


{-| Parse a simple string-valued meta event.
-}
parseMetaString : Int -> Decoder String
parseMetaString target =
    String.fromList
        -- <$> (bchar target *> varInt `andThen` (\l -> count l anyChar))
        <$> (bChar target *> varInt >>= (\l -> count l anyChar))


{-| Parse a meta event valued as a List of Bytes (masquerading as Ints).
-}
parseMetaBytes : Int -> Decoder (List Byte)
parseMetaBytes target =
    List.map toCode
        <$> (bChar target *> varInt >>= (\l -> count l anyChar))


eventText : Decoder Midi.Event
eventText =
    Text <$> parseMetaString 0x01 <?> "text"


eventCopyright : Decoder Midi.Event
eventCopyright =
    Copyright <$> parseMetaString 0x02 <?> "copyright"


eventTrackName : Decoder Midi.Event
eventTrackName =
    TrackName <$> parseMetaString 0x03 <?> "track name"


eventInstrumentName : Decoder Midi.Event
eventInstrumentName =
    InstrumentName <$> parseMetaString 0x04 <?> "instrument name"


eventLyrics : Decoder Midi.Event
eventLyrics =
    Lyrics <$> parseMetaString 0x05 <?> "lyrics"


eventMarker : Decoder Midi.Event
eventMarker =
    Marker <$> parseMetaString 0x06 <?> "marker"


eventCuePoint : Decoder Midi.Event
eventCuePoint =
    CuePoint <$> parseMetaString 0x07 <?> "cue point"


eventChannelPrefix : Decoder Midi.Event
eventChannelPrefix =
    ChannelPrefix <$> (bChar 0x20 *> bChar 0x01 *> uInt8 <?> "channel prefix")


eventTempoChange : Decoder Midi.Event
eventTempoChange =
    Tempo <$> (bChar 0x51 *> bChar 0x03 *> uInt24) <?> "tempo change"


eventSMPTEOffset : Decoder Midi.Event
eventSMPTEOffset =
    bChar 0x54 *> bChar 0x03 *> (SMPTEOffset <$> uInt8 <*> uInt8 <*> uInt8 <*> uInt8 <*> uInt8 <?> "SMTPE offset")


eventTimeSignature : Decoder Midi.Event
eventTimeSignature =
    bChar 0x58 *> bChar 0x04 *> (buildTimeSig <$> uInt8 <*> uInt8 <*> uInt8 <*> uInt8) <?> "time signature"


eventKeySignature : Decoder Midi.Event
eventKeySignature =
    bChar 0x59 *> bChar 0x02 *> (KeySignature <$> signedInt8 <*> uInt8)


eventSequencerSpecific : Decoder Midi.Event
eventSequencerSpecific =
    SequencerSpecific <$> parseMetaBytes 0x7F <?> "sequencer specific"


{-| A SysEx event is introduced by an 0xF0 byte and is followed by an array of bytes.
In Web Midi a sysex event starts with a 0xF0 byte and ends with an EOX (0xF7) byte.
There are also escaped SysEx messages, but these are only found in MIDI files.
-}
eventSysEx : Decoder Midi.Event
eventSysEx =
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


{-| A SysEx event in a file is introduced by an 0xF0 or 0xF7 byte, followed by a
variable length integer that denotes how many data bytes follow.
If it starts with 0xF0 the data bytes must be valid sysex data, however if it
starts with 0xF7 any data may follow.
Note: Since this library doesn't do anything special to handle multi-part
SysEx messages it must record the EOX byte as part of the SysEx message
here as opposed to for SysEx MIDI events where that byte can be left
implicit.
-}
eventFileSysExEvent : Decoder Midi.Event
eventFileSysExEvent =
    let
        parseFlavour : Decoder SysExFlavour
        parseFlavour =
            (bChar 0xF0 $> F0) <|> (bChar 0xF7 $> F7) <?> "sysex flavour"

        sysexData : Decoder Char
        sysexData =
            satisfy (\c -> toCode c < 128)

        parseUnescapedSysex : Decoder Midi.Event
        parseUnescapedSysex =
            SysEx
                <$> (bChar 0xF0 $> F0)
                <*> (List.map toCode
                        <$> (varInt
                                >>= (\n -> count n sysexData)
                            )
                    )
                <?> "unescaped system exclusive"

        parseEscapedSysex : Decoder Midi.Event
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


{-| Parse an unspecified meta event.
The possible range for the type is 00-7F. Not all values in this range are defined, but programs must be able
to cope with (ie ignore) unexpected values by examining the length and skipping over the data portion.
We cope by accepting any value here except TrackEnd which is the terminating condition for the list of MidiEvents
and so must not be recognized here.
-}
eventUnspecified : Decoder Midi.Event
eventUnspecified =
    Unspecified <$> notTrackEnd <*> (uInt8 >>= (\l -> count l uInt8))


{-| Parse an entire Track End message - not simply the event.
-}
trackEndMessage : Decoder ()
trackEndMessage =
    varInt *> bChar 0xFF *> bChar 0x2F *> bChar 0x00 *> succeed () <?> "track end"



-- Channel Parsers


eventNoteOn : Decoder Midi.Event
eventNoteOn =
    buildNote <$> bRange 0x90 0x9F <*> uInt8 <*> uInt8 <?> "note on"


eventNoteOff : Decoder Midi.Event
eventNoteOff =
    buildNoteOff <$> bRange 0x80 0x8F <*> uInt8 <*> uInt8 <?> "note off"


eventNoteAfterTouch : Decoder Midi.Event
eventNoteAfterTouch =
    buildNoteAfterTouch <$> bRange 0xA0 0xAF <*> uInt8 <*> uInt8 <?> "note after touch"


eventControlChange : Decoder Midi.Event
eventControlChange =
    buildControlChange <$> bRange 0xB0 0xBF <*> uInt8 <*> uInt8 <?> "control change"


eventProgramChange : Decoder Midi.Event
eventProgramChange =
    buildProgramChange <$> bRange 0xC0 0xCF <*> uInt8 <?> "program change"


eventChannelAfterTouch : Decoder Midi.Event
eventChannelAfterTouch =
    buildChannelAfterTouch <$> bRange 0xD0 0xDF <*> uInt8 <?> "channel after touch"


eventPitchBend : Decoder Midi.Event
eventPitchBend =
    buildPitchBend <$> bRange 0xE0 0xEF <*> uInt8 <*> uInt8 <?> "pitch bend"


{-| Running status is somewhat anomalous. It inherits the 'type' of the last event parsed,
(here called the parent) which must be a channel event.
We now macro-expand the running status message to be the type (and use the channel status)
of the parent. If the parent is missing or is not a channel event, we fail the parse.
-}
eventRunningStatus : Maybe Midi.Event -> Decoder Midi.Event
eventRunningStatus parent =
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


{-| Build NoteOn (unless the velocity is zero in which case NoteOff).
-}
buildNote : Int -> Int -> Int -> Midi.Event
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


{-| Abstract builders that construct MidiEvents that all have the same shape.
-}
channelBuilder3 : (Int -> Int -> Int -> Midi.Event) -> Int -> Int -> Int -> Midi.Event
channelBuilder3 construct cmd x y =
    let
        channel =
            -- cmd `and` 0x0F
            and cmd 0x0F
    in
    construct channel x y


channelBuilder2 : (Int -> Int -> Midi.Event) -> Int -> Int -> Midi.Event
channelBuilder2 construct cmd x =
    let
        channel =
            -- cmd `and` 0x0F
            and cmd 0x0F
    in
    construct channel x


{-| Build NoteOff.
-}
buildNoteOff : Int -> Int -> Int -> Midi.Event
buildNoteOff cmd note velocity =
    channelBuilder3 NoteOff cmd note velocity


{-| Build Note AfterTouch AKA Polyphonic Key Pressure.
-}
buildNoteAfterTouch : Int -> Int -> Int -> Midi.Event
buildNoteAfterTouch cmd note pressure =
    channelBuilder3 NoteAfterTouch cmd note pressure


{-| Build Control Change.
-}
buildControlChange : Int -> Int -> Int -> Midi.Event
buildControlChange cmd num value =
    channelBuilder3 ControlChange cmd num value


{-| Build Program Change.
-}
buildProgramChange : Int -> Int -> Midi.Event
buildProgramChange cmd num =
    channelBuilder2 ProgramChange
        cmd
        num


{-| Build Channel AfterTouch AKA Channel Key Pressure.
-}
buildChannelAfterTouch : Int -> Int -> Midi.Event
buildChannelAfterTouch cmd num =
    channelBuilder2 ChannelAfterTouch cmd num


{-| Build Pitch Bend.
-}
buildPitchBend : Int -> Int -> Int -> Midi.Event
buildPitchBend cmd lsb msb =
    channelBuilder2 PitchBend cmd <| lsb + shiftLeftBy 7 msb


{-| Build a Time Signature.
-}
buildTimeSig : Int -> Int -> Int -> Int -> Midi.Event
buildTimeSig nn dd cc bb =
    let
        denom =
            2 ^ dd
    in
    TimeSignature nn denom cc bb



-- Helpers


{-| Consume the overspill from a non-standard size chunk.
Actual is the parsed actual chunk size followed by the chunk contents (which are returned).
Expected is the expected size of the chunk.
Consume the rest if the difference suggests an overspill of unwanted chunk material.
-}
consumeOverspill : Decoder ( Int, a ) -> Int -> Decoder a
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



-- Helpers


{-| Variable length integers.
-}
varInt : Decoder Int
varInt =
    let
        helper : Decoder (List Int)
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
bRange : Int -> Int -> Decoder Int
bRange l r =
    let
        f a =
            toCode a >= l && toCode a <= r
    in
    toCode <$> satisfy f


notTrackEnd : Decoder Int
notTrackEnd =
    let
        c =
            fromCode 0x2F
    in
    toCode <$> noneOf [ c ]
