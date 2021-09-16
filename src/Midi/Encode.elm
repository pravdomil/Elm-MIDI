module Midi.Encode exposing (file, track, message, event)

{-| Module for encoding MIDI.

@docs file, track, message, event

-}

import Bitwise
import Bytes exposing (Bytes)
import Bytes.Encode as Encode
import Midi


type Error
    = NotSupportedEvent String


{-| Encode MIDI file.
-}
file : Midi.File -> Result Error Bytes
file a =
    let
        format : Int
        format =
            case a.format of
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
                , Encode.unsignedInt16 endianness format
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
message a =
    event a.event
        |> Result.map
            (\v ->
                [ varInt a.delta
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
            Err (NotSupportedEvent "SequenceNumber")

        Midi.Text _ ->
            Err (NotSupportedEvent "Text")

        Midi.Copyright _ ->
            Err (NotSupportedEvent "Copyright")

        Midi.TrackName _ ->
            Err (NotSupportedEvent "TrackName")

        Midi.InstrumentName _ ->
            Err (NotSupportedEvent "InstrumentName")

        Midi.Lyrics _ ->
            Err (NotSupportedEvent "Lyrics")

        Midi.Marker _ ->
            Err (NotSupportedEvent "Marker")

        Midi.CuePoint _ ->
            Err (NotSupportedEvent "CuePoint")

        Midi.ChannelPrefix _ ->
            Err (NotSupportedEvent "ChannelPrefix")

        Midi.EndOfTrack ->
            Ok []

        Midi.Tempo _ ->
            Err (NotSupportedEvent "Tempo")

        Midi.SMPTEOffset _ _ _ _ _ ->
            Err (NotSupportedEvent "SMPTEOffset")

        Midi.TimeSignature _ _ _ _ ->
            Err (NotSupportedEvent "TimeSignature")

        Midi.KeySignature _ _ ->
            Err (NotSupportedEvent "KeySignature")

        Midi.SequencerSpecific _ ->
            Err (NotSupportedEvent "SequencerSpecific")

        Midi.Unknown _ _ ->
            Err (NotSupportedEvent "Unknown")

        --
        Midi.NoteOff channel note velocity ->
            Ok
                [ Encode.unsignedInt8 (0x80 + channel)
                , Encode.unsignedInt8 note
                , Encode.unsignedInt8 velocity
                ]

        Midi.NoteOn channel note velocity ->
            Ok
                [ Encode.unsignedInt8 (0x90 + channel)
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

        --
        Midi.SysEx bytes ->
            Ok
                [ Encode.unsignedInt8 0xF0
                , Encode.bytes bytes
                , Encode.unsignedInt8 0xF7
                ]
    )
        |> Result.map (Encode.sequence >> Encode.encode)



-- Helpers


endianness : Bytes.Endianness
endianness =
    Bytes.BE


varInt : Int -> Encode.Encoder
varInt a =
    -- TODO use Bytes module instead of Int
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
