module Midi.Encode exposing (recording, track, message, event)

{-| Module for encoding MIDI.

@docs recording, track, message, event

-}

import Bitwise
import Bytes exposing (Bytes)
import Bytes.Encode as Encode
import Midi


type Error
    = NotSupported String


{-| Encode MIDI recording.
-}
recording : Midi.Recording -> Result Error Bytes
recording a =
    let
        trackType : Int
        trackType =
            case a.trackType of
                Midi.Simultaneous ->
                    1

                Midi.Independent ->
                    2

        encodedTracks : Result Error (List Encode.Encoder)
        encodedTracks =
            (Tuple.first a.tracks :: Tuple.second a.tracks)
                |> List.map (track >> Result.map Encode.bytes)
                |> resultSequence
    in
    encodedTracks
        |> Result.map
            (\v ->
                [ Encode.string "MThd"
                , Encode.unsignedInt32 endianness 6
                , Encode.unsignedInt16 endianness trackType
                , Encode.unsignedInt16 endianness (List.length v)
                , Encode.unsignedInt16 endianness a.tempo
                , Encode.sequence v
                ]
                    |> Encode.sequence
                    |> Encode.encode
            )


{-| Encode MIDI track.
-}
track : Midi.Track -> Result Error Bytes
track a =
    let
        encodedMsgs : Result Error (List Encode.Encoder)
        encodedMsgs =
            a
                |> List.map (message >> Result.map Encode.bytes)
                |> resultSequence
                |> Result.map
                    (\v ->
                        [ Encode.sequence v
                        , Encode.unsignedInt8 0x00
                        , Encode.unsignedInt8 0xFF
                        , Encode.unsignedInt8 0x2F
                        , Encode.unsignedInt8 0x00
                        ]
                    )
    in
    encodedMsgs
        |> Result.map
            (\v ->
                [ Encode.string "MTrk"
                , Encode.unsignedInt32 endianness (List.length v)
                , Encode.sequence v
                ]
                    |> Encode.sequence
                    |> Encode.encode
            )


{-| Encode MIDI message.
-}
message : Midi.Message -> Result Error Bytes
message ( ticks, a ) =
    event a
        |> Result.map
            (\v ->
                [ varInt ticks
                , Encode.bytes v
                ]
                    |> Encode.sequence
                    |> Encode.encode
            )


{-| Encode MIDI event.
-}
event : Midi.Event -> Result Error Bytes
event a =
    (case a of
        Midi.SequenceNumber _ ->
            Err (NotSupported "SequenceNumber")

        Midi.Text _ ->
            Err (NotSupported "Text")

        Midi.Copyright _ ->
            Err (NotSupported "Copyright")

        Midi.TrackName _ ->
            Err (NotSupported "TrackName")

        Midi.InstrumentName _ ->
            Err (NotSupported "InstrumentName")

        Midi.Lyrics _ ->
            Err (NotSupported "Lyrics")

        Midi.Marker _ ->
            Err (NotSupported "Marker")

        Midi.CuePoint _ ->
            Err (NotSupported "CuePoint")

        Midi.ChannelPrefix _ ->
            Err (NotSupported "ChannelPrefix")

        Midi.Tempo _ ->
            Err (NotSupported "Tempo")

        Midi.SMPTEOffset _ _ _ _ _ ->
            Err (NotSupported "SMPTEOffset")

        Midi.TimeSignature _ _ _ _ ->
            Err (NotSupported "TimeSignature")

        Midi.KeySignature _ _ ->
            Err (NotSupported "KeySignature")

        Midi.SequencerSpecific _ ->
            Err (NotSupported "SequencerSpecific")

        Midi.SysEx bytes ->
            Ok
                [ Encode.unsignedInt8 0xF0
                , Encode.bytes bytes
                , Encode.unsignedInt8 0xF7
                ]

        Midi.Unspecified _ _ ->
            Err (NotSupported "Unspecified")

        Midi.NoteOn channel note velocity ->
            Ok
                [ Encode.unsignedInt8 (0x90 + channel)
                , Encode.unsignedInt8 note
                , Encode.unsignedInt8 velocity
                ]

        Midi.NoteOff channel note velocity ->
            Ok
                [ Encode.unsignedInt8 (0x80 + channel)
                , Encode.unsignedInt8 note
                , Encode.unsignedInt8 velocity
                ]

        Midi.NoteAfterTouch channel note velocity ->
            Ok
                [ Encode.unsignedInt8 (0xA0 + channel)
                , Encode.unsignedInt8 note
                , Encode.unsignedInt8 velocity
                ]

        Midi.ControlChange channel controllerNumber value ->
            Ok
                [ Encode.unsignedInt8 (0xB0 + channel)
                , Encode.unsignedInt8 controllerNumber
                , Encode.unsignedInt8 value
                ]

        Midi.ProgramChange channel value ->
            Ok
                [ Encode.unsignedInt8 (0xC0 + channel)
                , Encode.unsignedInt8 value
                ]

        Midi.ChannelAfterTouch channel velocity ->
            Ok
                [ Encode.unsignedInt8 (0xD0 + channel)
                , Encode.unsignedInt8 velocity
                ]

        Midi.PitchBend channel bend ->
            let
                lower : Int
                lower =
                    Bitwise.and bend 127

                upper : Int
                upper =
                    Bitwise.shiftRightBy 7 bend
            in
            Ok
                [ Encode.unsignedInt8 (0xE0 + channel)
                , Encode.unsignedInt8 lower
                , Encode.unsignedInt8 upper
                ]
    )
        |> Result.map (Encode.sequence >> Encode.encode)



-- Helpers


endianness : Bytes.Endianness
endianness =
    Bytes.BE


varInt : Int -> Encode.Encoder
varInt a =
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
        |> List.map Encode.signedInt8
        |> Encode.sequence


resultSequence : List (Result x a) -> Result x (List a)
resultSequence a =
    List.foldr (Result.map2 (::)) (Ok []) a
