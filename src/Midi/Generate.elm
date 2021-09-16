module Midi.Generate exposing (event, recording)

{-| Module for encoding MIDI types as "binary"


# API Reference

@docs event, recording

-}

import Bitwise
import Midi


{-| Generate a MIDI event
-}
event : Midi.Event -> List Int
event a =
    case a of
        Midi.SysEx Midi.F0 bytes ->
            0xF0 :: (bytes ++ [ Midi.endOfExclusive ])

        Midi.SysEx Midi.F7 bytes ->
            Debug.crash "WebMIDI SysEx events should all be of the 0xF0 flavor."

        Midi.NoteOn channel note velocity ->
            [ 0x90 + channel, note, velocity ]

        Midi.NoteOff channel note velocity ->
            [ 0x80 + channel, note, velocity ]

        Midi.NoteAfterTouch channel note velocity ->
            [ 0xA0 + channel, note, velocity ]

        Midi.ControlChange channel controllerNumber value ->
            [ 0xB0 + channel, controllerNumber, value ]

        Midi.ProgramChange channel value ->
            [ 0xC0 + channel, value ]

        Midi.ChannelAfterTouch channel velocity ->
            [ 0xD0 + channel, velocity ]

        Midi.PitchBend channel bend ->
            let
                lower =
                    Bitwise.and bend 127

                upper =
                    Bitwise.shiftRightBy 7 bend
            in
            [ 0xE0 + channel, lower, upper ]

        _ ->
            Debug.crash "Unknown MIDI event."


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


strToBytes : String -> List Int
strToBytes =
    List.map Char.toCode << String.toList


uint16 : Int -> List Int
uint16 x =
    let
        b1 =
            Bitwise.and 255 (Bitwise.shiftRightBy 8 x)

        b2 =
            Bitwise.and 255 x
    in
    [ b1, b2 ]


uint32 : Int -> List Int
uint32 x =
    let
        b1 =
            Bitwise.and 255 (Bitwise.shiftRightBy 24 x)

        b2 =
            Bitwise.and 255 (Bitwise.shiftRightBy 16 x)

        b3 =
            Bitwise.and 255 (Bitwise.shiftRightBy 8 x)

        b4 =
            Bitwise.and 255 x
    in
    [ b1, b2, b3, b4 ]


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
