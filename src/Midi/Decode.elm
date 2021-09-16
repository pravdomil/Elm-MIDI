module Midi.Decode exposing (file, event)

{-| Module for decoding MIDI.

@docs file, event

-}

import Bitwise
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
    Decode.decode (eventDecoder Nothing) a



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
    Decode.map2 Midi.Message varInt (eventDecoder parent)



--


eventDecoder : Maybe Midi.Message -> Decoder Midi.Event
eventDecoder parent =
    Decode.unsignedInt8
        |> Decode.andThen
            (\v ->
                case ( bitwiseClear 0x0F v, v ) of
                    ( 0x80, _ ) ->
                        noteOffEvent

                    ( 0x90, _ ) ->
                        noteOnEvent

                    ( 0xA0, _ ) ->
                        noteAfterTouchEvent

                    ( 0xB0, _ ) ->
                        controlChangeEvent

                    ( 0xC0, _ ) ->
                        programChangeEvent

                    ( 0xD0, _ ) ->
                        channelAfterTouchEvent

                    ( 0xE0, _ ) ->
                        pitchBendEvent

                    ( _, 0xFF ) ->
                        metaEvent

                    ( _, 0xF0 ) ->
                        sysExEvent

                    _ ->
                        runningStatusEvent (Maybe.map .event parent)
            )


metaEvent : Decoder Midi.Event
metaEvent =
    Decode.unsignedInt8
        |> Decode.andThen
            (\v ->
                case v of
                    0x00 ->
                        sequenceNumberEvent

                    0x01 ->
                        textEvent

                    0x02 ->
                        copyrightEvent

                    0x03 ->
                        trackNameEvent

                    0x04 ->
                        instrumentNameEvent

                    0x05 ->
                        lyricsEvent

                    0x06 ->
                        markerEvent

                    0x07 ->
                        cuePointEvent

                    0x20 ->
                        channelPrefixEvent

                    0x2F ->
                        endOfTrackEvent

                    0x51 ->
                        tempoEvent

                    0x54 ->
                        smpteOffsetEvent

                    0x58 ->
                        timeSignatureEvent

                    0x59 ->
                        keySignatureEvent

                    0x7F ->
                        sequencerSpecificEvent

                    _ ->
                        unknownEvent
            )



--


endOfTrackEvent : Decoder Midi.Event
endOfTrackEvent =
    bChar 0x2F *> bChar 0x00 *> succeed Nothing <?> "sequence number"


sequenceNumberEvent : Decoder Midi.Event
sequenceNumberEvent =
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


textEvent : Decoder Midi.Event
textEvent =
    Text <$> parseMetaString 0x01 <?> "text"


copyrightEvent : Decoder Midi.Event
copyrightEvent =
    Copyright <$> parseMetaString 0x02 <?> "copyright"


trackNameEvent : Decoder Midi.Event
trackNameEvent =
    TrackName <$> parseMetaString 0x03 <?> "track name"


instrumentNameEvent : Decoder Midi.Event
instrumentNameEvent =
    InstrumentName <$> parseMetaString 0x04 <?> "instrument name"


lyricsEvent : Decoder Midi.Event
lyricsEvent =
    Lyrics <$> parseMetaString 0x05 <?> "lyrics"


markerEvent : Decoder Midi.Event
markerEvent =
    Marker <$> parseMetaString 0x06 <?> "marker"


cuePointEvent : Decoder Midi.Event
cuePointEvent =
    CuePoint <$> parseMetaString 0x07 <?> "cue point"


channelPrefixEvent : Decoder Midi.Event
channelPrefixEvent =
    ChannelPrefix <$> (bChar 0x20 *> bChar 0x01 *> uInt8 <?> "channel prefix")


tempoEvent : Decoder Midi.Event
tempoEvent =
    Tempo <$> (bChar 0x51 *> bChar 0x03 *> uInt24) <?> "tempo change"


smpteOffsetEvent : Decoder Midi.Event
smpteOffsetEvent =
    bChar 0x54 *> bChar 0x03 *> (SMPTEOffset <$> uInt8 <*> uInt8 <*> uInt8 <*> uInt8 <*> uInt8 <?> "SMTPE offset")


timeSignatureEvent : Decoder Midi.Event
timeSignatureEvent =
    bChar 0x58 *> bChar 0x04 *> (buildTimeSig <$> uInt8 <*> uInt8 <*> uInt8 <*> uInt8) <?> "time signature"


keySignatureEvent : Decoder Midi.Event
keySignatureEvent =
    bChar 0x59 *> bChar 0x02 *> (KeySignature <$> signedInt8 <*> uInt8)


sequencerSpecificEvent : Decoder Midi.Event
sequencerSpecificEvent =
    SequencerSpecific <$> parseMetaBytes 0x7F <?> "sequencer specific"


{-| A SysEx event is introduced by an 0xF0 byte and is followed by an array of bytes.
In Web Midi a sysex event starts with a 0xF0 byte and ends with an EOX (0xF7) byte.
There are also escaped SysEx messages, but these are only found in MIDI files.
-}
sysExEvent : Decoder Midi.Event
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
unknownEvent : Decoder Midi.Event
unknownEvent =
    Unspecified <$> notTrackEnd <*> (uInt8 >>= (\l -> count l uInt8))


{-| Parse an entire Track End message - not simply the event.
-}
trackEndMessage : Decoder ()
trackEndMessage =
    varInt *> bChar 0xFF *> bChar 0x2F *> bChar 0x00 *> succeed () <?> "track end"



-- Channel Parsers


noteOffEvent : Decoder Midi.Event
noteOffEvent =
    buildNoteOff <$> bRange 0x80 0x8F <*> uInt8 <*> uInt8 <?> "note off"


noteOnEvent : Decoder Midi.Event
noteOnEvent =
    buildNote <$> bRange 0x90 0x9F <*> uInt8 <*> uInt8 <?> "note on"


noteAfterTouchEvent : Decoder Midi.Event
noteAfterTouchEvent =
    buildNoteAfterTouch <$> bRange 0xA0 0xAF <*> uInt8 <*> uInt8 <?> "note after touch"


controlChangeEvent : Decoder Midi.Event
controlChangeEvent =
    buildControlChange <$> bRange 0xB0 0xBF <*> uInt8 <*> uInt8 <?> "control change"


programChangeEvent : Decoder Midi.Event
programChangeEvent =
    buildProgramChange <$> bRange 0xC0 0xCF <*> uInt8 <?> "program change"


channelAfterTouchEvent : Decoder Midi.Event
channelAfterTouchEvent =
    buildChannelAfterTouch <$> bRange 0xD0 0xDF <*> uInt8 <?> "channel after touch"


pitchBendEvent : Decoder Midi.Event
pitchBendEvent =
    buildPitchBend <$> bRange 0xE0 0xEF <*> uInt8 <*> uInt8 <?> "pitch bend"


{-| Running status is somewhat anomalous. It inherits the 'type' of the last event parsed,
(here called the parent) which must be a channel event.
We now macro-expand the running status message to be the type (and use the channel status)
of the parent. If the parent is missing or is not a channel event, we fail the parse.
-}
runningStatusEvent : Maybe Midi.Event -> Decoder Midi.Event
runningStatusEvent parent =
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


bitwiseClear : Int -> Int -> Int
bitwiseClear mask a =
    Bitwise.and (Bitwise.complement mask) a


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
