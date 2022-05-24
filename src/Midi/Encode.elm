module Midi.Encode exposing (file, track, event, eventType)

{-| Module for encoding MIDI.

@docs file, track, event, eventType

-}

import Bitwise
import Bytes
import Bytes.Encode as Encode
import Bytes.Encode.Extra
import Midi


{-| Encode MIDI file.
-}
file : Midi.File -> Encode.Encoder
file a =
    let
        format : Int
        format =
            case a.format of
                Midi.Simultaneous ->
                    0

                Midi.Independent ->
                    2

        tracks : List Midi.Track
        tracks =
            Tuple.first a.tracks :: Tuple.second a.tracks

        header : Bytes.Bytes
        header =
            [ Encode.unsignedInt16 endianness format
            , Encode.unsignedInt16 endianness (List.length tracks)
            , Encode.unsignedInt16 endianness ((\(Midi.TicksPerBeat v) -> v) a.tempo)
            ]
                |> Encode.sequence
                |> Encode.encode
    in
    [ Encode.string "MThd"
    , Encode.unsignedInt32 endianness (header |> Bytes.width)
    , Encode.bytes header
    , Encode.sequence (tracks |> List.map track)
    ]
        |> Encode.sequence


{-| Encode MIDI track.
-}
track : Midi.Track -> Encode.Encoder
track a =
    let
        events : Bytes.Bytes
        events =
            Encode.sequence
                [ a |> List.map event |> Encode.sequence
                , event (Midi.Event (Midi.Ticks 0) Midi.EndOfTrack)
                ]
                |> Encode.encode
    in
    [ Encode.string "MTrk"
    , Encode.unsignedInt32 endianness (events |> Bytes.width)
    , Encode.bytes events
    ]
        |> Encode.sequence


{-| Encode MIDI event.
-}
event : Midi.Event -> Encode.Encoder
event a =
    [ encodeVariableInt ((\(Midi.Ticks v) -> v) a.delta)
    , eventType a.event
    ]
        |> Encode.sequence


{-| Encode MIDI event type.
-}
eventType : Midi.EventType -> Encode.Encoder
eventType a =
    let
        metaEvent : Int -> Encode.Encoder -> Encode.Encoder
        metaEvent type_ encoder =
            let
                bytes : Bytes.Bytes
                bytes =
                    Encode.encode encoder
            in
            Encode.sequence
                [ Encode.unsignedInt8 0xFF
                , Encode.unsignedInt8 type_
                , encodeVariableInt (bytes |> Bytes.width)
                , Encode.bytes bytes
                ]

        controlEvent : Int -> Midi.Channel -> Int -> Int -> Encode.Encoder
        controlEvent type_ (Midi.Channel channel) b c =
            Encode.sequence
                [ Encode.unsignedInt8 (channel + type_)
                , Encode.unsignedInt8 b
                , Encode.unsignedInt8 c
                ]

        controlEvent2 : Int -> Midi.Channel -> Int -> Encode.Encoder
        controlEvent2 type_ (Midi.Channel channel) b =
            Encode.sequence
                [ Encode.unsignedInt8 (channel + type_)
                , Encode.unsignedInt8 b
                ]
    in
    case a of
        Midi.SequenceNumber b ->
            metaEvent 0x00 (Encode.unsignedInt16 endianness b)

        Midi.Text b ->
            metaEvent 0x01 (Encode.string b)

        Midi.Copyright b ->
            metaEvent 0x02 (Encode.string b)

        Midi.TrackName b ->
            metaEvent 0x03 (Encode.string b)

        Midi.InstrumentName b ->
            metaEvent 0x04 (Encode.string b)

        Midi.Lyrics b ->
            metaEvent 0x05 (Encode.string b)

        Midi.Marker b ->
            metaEvent 0x06 (Encode.string b)

        Midi.CuePoint b ->
            metaEvent 0x07 (Encode.string b)

        Midi.ChannelPrefix b ->
            metaEvent 0x20 (Encode.unsignedInt8 b)

        Midi.EndOfTrack ->
            metaEvent 0x2F (Encode.sequence [])

        Midi.Tempo b ->
            metaEvent 0x51 (Bytes.Encode.Extra.unsignedInt24 endianness b)

        Midi.SMPTEOffset b c d e f ->
            metaEvent 0x54
                (Encode.sequence
                    [ Encode.signedInt8 b
                    , Encode.unsignedInt8 c
                    , Encode.unsignedInt8 d
                    , Encode.unsignedInt8 e
                    , Encode.unsignedInt8 f
                    ]
                )

        Midi.TimeSignature b c d e ->
            metaEvent 0x58
                (Encode.sequence
                    [ Encode.signedInt8 b
                    , Encode.unsignedInt8 c
                    , Encode.unsignedInt8 d
                    , Encode.unsignedInt8 e
                    ]
                )

        Midi.KeySignature b c ->
            metaEvent 0x59
                (Encode.sequence
                    [ Encode.signedInt8 b
                    , Encode.unsignedInt8 c
                    ]
                )

        Midi.SequencerSpecific b ->
            metaEvent 0x7F (Encode.bytes b)

        Midi.Unknown b c ->
            metaEvent b (Encode.bytes c)

        --
        Midi.NoteOff b (Midi.Note c) (Midi.Velocity d) ->
            controlEvent 0x80 b c d

        Midi.NoteOn b (Midi.Note c) (Midi.Velocity d) ->
            controlEvent 0x90 b c d

        Midi.NoteAfterTouch b (Midi.Note c) (Midi.Velocity d) ->
            controlEvent 0xA0 b c d

        Midi.ControllerChange b (Midi.ControllerNumber c) (Midi.Velocity d) ->
            controlEvent 0xB0 b c d

        Midi.ProgramChange b (Midi.ProgramNumber c) ->
            controlEvent2 0xC0 b c

        Midi.ChannelAfterTouch b (Midi.Velocity c) ->
            controlEvent2 0xD0 b c

        Midi.PitchBend b (Midi.Velocity c) ->
            controlEvent 0xE0 b (c |> Bitwise.shiftRightBy 8) c

        --
        Midi.SystemExclusive b ->
            Encode.sequence
                [ Encode.unsignedInt8 0xF0
                , encodeVariableInt (b |> Bytes.width)
                , Encode.bytes b
                ]



-- Helpers


encodeVariableInt : Int -> Encode.Encoder
encodeVariableInt a =
    -- todo use Bytes module instead of Int
    let
        helper : Int -> List Int -> List Int
        helper b bytes =
            if b < 128 then
                (b + 128) :: bytes

            else
                helper
                    (Bitwise.shiftRightBy 7 b)
                    ((128 + Bitwise.and 127 b) :: bytes)
    in
    (if a < 128 then
        [ a ]

     else
        helper (Bitwise.shiftRightBy 7 a) [ Bitwise.and 127 a ]
    )
        |> List.map Encode.unsignedInt8
        |> Encode.sequence


endianness : Bytes.Endianness
endianness =
    Bytes.BE
