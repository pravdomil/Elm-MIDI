module Midi.Encode exposing (event, recording)

{-| Module for encoding MIDI.

@docs event, recording

-}

import Bitwise
import Bytes exposing (Bytes)
import Bytes.Encode as Encode
import Midi


type Error
    = NotSupported


{-| Encode MIDI event.
-}
event : Midi.Event -> Result Error Bytes
event a =
    (case a of
        Midi.SequenceNumber _ ->
            Err NotSupported

        Midi.Text _ ->
            Err NotSupported

        Midi.Copyright _ ->
            Err NotSupported

        Midi.TrackName _ ->
            Err NotSupported

        Midi.InstrumentName _ ->
            Err NotSupported

        Midi.Lyrics _ ->
            Err NotSupported

        Midi.Marker _ ->
            Err NotSupported

        Midi.CuePoint _ ->
            Err NotSupported

        Midi.ChannelPrefix _ ->
            Err NotSupported

        Midi.Tempo _ ->
            Err NotSupported

        Midi.SMPTEOffset _ _ _ _ _ ->
            Err NotSupported

        Midi.TimeSignature _ _ _ _ ->
            Err NotSupported

        Midi.KeySignature _ _ ->
            Err NotSupported

        Midi.SequencerSpecific _ ->
            Err NotSupported

        Midi.SysEx bytes ->
            Ok
                [ Encode.unsignedInt8 0xF0
                , Encode.bytes bytes
                , Encode.unsignedInt8 0xF7
                ]

        Midi.Unspecified _ _ ->
            Err NotSupported

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


{-| Generate a MIDI recording
-}
recording : Midi.Recording -> List Int
recording midi =
    case midi of
        SingleTrack ticksPerBeat t ->
            header 0 1 ticksPerBeat ++ track t

        MultipleTracks tracksType ticksPerBeat ts ->
            let
                format =
                    case tracksType of
                        Midi.Simultaneous ->
                            1

                        Midi.Independent ->
                            2
            in
            header format (List.length ts) ticksPerBeat ++ List.concatMap track ts



-- Lower level generators


header : Int -> Int -> Int -> List Int
header format numTracks ticksPerBeat =
    List.concat
        [ strToBytes "MThd"
        , uint32 6
        , uint16 format
        , uint16 numTracks
        , uint16 ticksPerBeat
        ]


track : Midi.Track -> List Int
track t =
    let
        endOfTrack =
            [ 0x00, 0xFF, 0x2F, 0x00 ]

        encodedMsgs =
            List.concatMap midiMessage t ++ endOfTrack

        len =
            List.length encodedMsgs
    in
    strToBytes "MTrk" ++ uint32 len ++ encodedMsgs


midiMessage : Midi.Message -> List Int
midiMessage ( ticks, e ) =
    varInt ticks ++ fileEvent e


fileEvent : Midi.Event -> List Int
fileEvent e =
    case e of
        Midi.SysEx Midi.F0 bytes ->
            0xF0 :: (varInt (List.length bytes) ++ bytes)

        Midi.SysEx Midi.F7 bytes ->
            0xF7 :: (varInt (List.length bytes) ++ bytes)

        _ ->
            -- Use the regular event generator for everything else
            event e



-- Helper functions


varInt : Int -> List Int
varInt x =
    let
        varIntHelper x bytes =
            if x < 128 then
                (x + 128) :: bytes

            else
                varIntHelper
                    (Bitwise.shiftRightBy 7 x)
                    ((128 + Bitwise.and 127 x) :: bytes)
    in
    if x < 128 then
        [ x ]

    else
        varIntHelper (Bitwise.shiftRightBy 7 x) [ Bitwise.and 127 x ]
