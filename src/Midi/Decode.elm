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


{-| Decode MIDI event.
-}
event : Bytes -> Maybe Midi.Event
event a =
    Decode.decode (midiEvent Nothing) a



--


fileDecoder : Decoder Midi.File
fileDecoder =
    decodeConst "MThd"
        |> Decode.andThen
            (\_ ->
                decodeBlock 6
                    (Decode.map3
                        (\v1 v2 v3 ->
                            { format = v1
                            , trackCount = v2
                            , tempo = v3
                            }
                        )
                        (Decode.unsignedInt16 endianness)
                        (Decode.unsignedInt16 endianness)
                        (Decode.unsignedInt16 endianness)
                    )
            )
        |> Decode.andThen
            (\v ->
                case v.format of
                    0 ->
                        tracks v.trackCount |> Decode.map (Midi.File v.tempo Midi.Simultaneous)

                    1 ->
                        tracks v.trackCount |> Decode.map (Midi.File v.tempo Midi.Simultaneous)

                    2 ->
                        tracks v.trackCount |> Decode.map (Midi.File v.tempo Midi.Independent)

                    _ ->
                        Decode.fail
            )


tracks : Int -> Decoder ( Midi.Track, List Midi.Track )
tracks trackCount =
    Decode.loop ( 0, [] )
        (\( i, acc ) ->
            if i == trackCount then
                Decode.succeed (Decode.Done (List.reverse acc))

            else
                track
                    |> Decode.map
                        (\v ->
                            Decode.Loop ( i + 1, v :: acc )
                        )
        )
        |> Decode.andThen
            (\v ->
                case v of
                    first :: rest ->
                        Decode.succeed ( first, rest )

                    _ ->
                        Decode.fail
            )


track : Decoder Midi.Track
track =
    decodeConst "MTrk"
        |> Decode.andThen (\_ -> Decode.unsignedInt32 endianness)
        |> Decode.andThen (\_ -> messages)


messages : Decoder (List Midi.Message)
messages =
    Decode.loop []
        (\acc ->
            message (List.head acc)
                |> Decode.map
                    (\v ->
                        if v.event == Midi.EndOfTrack then
                            Decode.Done (List.reverse acc)

                        else
                            Decode.Loop (v :: acc)
                    )
        )


message : Maybe Midi.Message -> Decoder Midi.Message
message parent =
    Decode.map2 Midi.Message varInt (midiEvent parent)


midiEvent : Maybe Midi.Event -> Decoder Midi.Event
midiEvent parent =
    choice
        [ eventMeta
        , eventNoteOn
        , eventNoteOff
        , eventNoteAfterTouch
        , eventControlChange
        , eventProgramChange
        , eventChannelAfterTouch
        , eventPitchBend
        , eventFileSysExEvent
        , eventRunningStatus parent
        ]


eventMeta : Decoder (Maybe Midi.Event)
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
            , eventEndOfTrack
            , eventUnspecified
            ]



--


eventEndOfTrack : Decoder (Maybe Midi.Event)
eventEndOfTrack =
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



-- Helpers


endianness : Bytes.Endianness
endianness =
    Bytes.BE


decodeBlock : Int -> Decoder a -> Decoder a
decodeBlock sizeOfDecoder decoder =
    Decode.unsignedInt32 endianness
        |> Decode.andThen
            (\v ->
                decoder
                    |> Decode.andThen
                        (\vv ->
                            Decode.bytes (v - sizeOfDecoder)
                                |> Decode.map (\_ -> vv)
                        )
            )


decodeConst : String -> Decoder ()
decodeConst a =
    Decode.string (String.length a)
        |> Decode.andThen
            (\v ->
                if v == a then
                    Decode.succeed ()

                else
                    Decode.fail
            )
