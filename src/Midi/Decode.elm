module Midi.Decode exposing (file, eventType)

{-| Module for decoding MIDI.

@docs file, eventType

-}

import Bitwise
import Bytes exposing (Bytes)
import Bytes.Decode as Decode exposing (Decoder)
import Bytes.Decode.Extra
import Midi


{-| Decode MIDI file.
-}
file : Bytes -> Maybe Midi.File
file a =
    Decode.decode fileDecoder a


{-| Decode MIDI event.
-}
eventType : Bytes -> Maybe Midi.EventType
eventType a =
    Decode.decode (eventType_ Nothing) a



--


fileDecoder : Decoder Midi.File
fileDecoder =
    let
        headerDecoder : Decoder ( Midi.Format, Int, Midi.TicksPerBeat )
        headerDecoder =
            Decode.map3
                (\v1 v2 v3 ->
                    ( v1, v2, v3 )
                )
                (Decode.unsignedInt16 endianness
                    |> Decode.andThen
                        (\v ->
                            case v of
                                0 ->
                                    Decode.succeed Midi.Simultaneous

                                1 ->
                                    Decode.succeed Midi.Simultaneous

                                2 ->
                                    Decode.succeed Midi.Independent

                                _ ->
                                    Decode.fail
                        )
                )
                (Decode.unsignedInt16 endianness)
                (Decode.unsignedInt16 endianness |> Decode.map Midi.TicksPerBeat)
    in
    decodeStringConst "MThd"
        |> Decode.andThen (\_ -> Decode.unsignedInt32 endianness)
        |> Decode.andThen (\v -> decodeChunk v headerDecoder)
        |> Decode.andThen
            (\( v1, v2, v3 ) ->
                Decode.map (Midi.File v1 v3) (tracks v2)
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
    decodeStringConst "MTrk"
        |> Decode.andThen (\_ -> Decode.unsignedInt32 endianness)
        |> Decode.andThen (\v -> decodeChunk v events)


events : Decoder (List Midi.Event)
events =
    Decode.loop []
        (\acc ->
            event (List.head acc)
                |> Decode.map
                    (\v ->
                        if v.event == Midi.EndOfTrack then
                            Decode.Done (List.reverse acc)

                        else
                            Decode.Loop (v :: acc)
                    )
        )


event : Maybe Midi.Event -> Decoder Midi.Event
event previous =
    Decode.map2 Midi.Event (Decode.map Midi.Ticks variableInt) (eventType_ previous)



--


eventType_ : Maybe Midi.Event -> Decoder Midi.EventType
eventType_ previous =
    Decode.unsignedInt8
        |> Decode.andThen
            (\v ->
                let
                    channel : Midi.Channel
                    channel =
                        Bitwise.and 0x0F v |> Midi.Channel
                in
                case Bitwise.shiftRightBy 4 v of
                    0x08 ->
                        Decode.map2
                            (Midi.NoteOff channel)
                            (Decode.unsignedInt8 |> Decode.map Midi.Note)
                            (Decode.unsignedInt8 |> Decode.map Midi.Velocity)

                    0x09 ->
                        Decode.map2
                            (Midi.NoteOn channel)
                            (Decode.unsignedInt8 |> Decode.map Midi.Note)
                            (Decode.unsignedInt8 |> Decode.map Midi.Velocity)

                    0x0A ->
                        Decode.map2
                            (Midi.NoteAfterTouch channel)
                            (Decode.unsignedInt8 |> Decode.map Midi.Note)
                            (Decode.unsignedInt8 |> Decode.map Midi.Velocity)

                    0x0B ->
                        Decode.map2
                            (Midi.ControllerChange channel)
                            (Decode.unsignedInt8 |> Decode.map Midi.ControllerNumber)
                            (Decode.unsignedInt8 |> Decode.map Midi.Velocity)

                    0x0C ->
                        Decode.map
                            (Midi.ProgramChange channel)
                            (Decode.unsignedInt8 |> Decode.map Midi.ProgramNumber)

                    0x0D ->
                        Decode.map
                            (Midi.ChannelAfterTouch channel)
                            (Decode.unsignedInt8 |> Decode.map Midi.Velocity)

                    0x0E ->
                        Decode.map
                            (Midi.PitchBend channel)
                            (Decode.unsignedInt16 endianness |> Decode.map Midi.Velocity)

                    0x0F ->
                        case v of
                            0xFF ->
                                metaEventType

                            0xF0 ->
                                systemExclusiveEventType

                            0xF7 ->
                                systemExclusiveEventType

                            _ ->
                                Decode.fail

                    _ ->
                        case previous of
                            Just a ->
                                case a.event of
                                    Midi.NoteOff channel_ _ _ ->
                                        Decode.map
                                            (Midi.NoteOff channel_ (Midi.Note v))
                                            (Decode.unsignedInt8 |> Decode.map Midi.Velocity)

                                    Midi.NoteOn channel_ _ _ ->
                                        Decode.map
                                            (Midi.NoteOn channel_ (Midi.Note v))
                                            (Decode.unsignedInt8 |> Decode.map Midi.Velocity)

                                    Midi.NoteAfterTouch channel_ _ _ ->
                                        Decode.map
                                            (Midi.NoteAfterTouch channel_ (Midi.Note v))
                                            (Decode.unsignedInt8 |> Decode.map Midi.Velocity)

                                    Midi.ControllerChange channel_ _ _ ->
                                        Decode.map
                                            (Midi.ControllerChange channel_ (Midi.ControllerNumber v))
                                            (Decode.unsignedInt8 |> Decode.map Midi.Velocity)

                                    Midi.ProgramChange channel_ _ ->
                                        Decode.succeed (Midi.ProgramChange channel_ (Midi.ProgramNumber v))

                                    Midi.ChannelAfterTouch channel_ _ ->
                                        Decode.succeed (Midi.ChannelAfterTouch channel_ (Midi.Velocity v))

                                    Midi.PitchBend channel_ _ ->
                                        Decode.succeed (Midi.PitchBend channel_ (Midi.Velocity v))

                                    _ ->
                                        Decode.fail

                            Nothing ->
                                Decode.fail
            )


metaEventType : Decoder Midi.EventType
metaEventType =
    let
        decoder : Int -> Int -> Decoder Midi.EventType
        decoder type_ length =
            case type_ of
                0x00 ->
                    Decode.map Midi.SequenceNumber (Decode.unsignedInt16 endianness)

                0x01 ->
                    Decode.map Midi.Text (Decode.string length)

                0x02 ->
                    Decode.map Midi.Copyright (Decode.string length)

                0x03 ->
                    Decode.map Midi.TrackName (Decode.string length)

                0x04 ->
                    Decode.map Midi.InstrumentName (Decode.string length)

                0x05 ->
                    Decode.map Midi.Lyrics (Decode.string length)

                0x06 ->
                    Decode.map Midi.Marker (Decode.string length)

                0x07 ->
                    Decode.map Midi.CuePoint (Decode.string length)

                0x20 ->
                    Decode.map Midi.ChannelPrefix Decode.unsignedInt8

                0x2F ->
                    Decode.succeed Midi.EndOfTrack

                0x51 ->
                    Decode.map Midi.Tempo (Bytes.Decode.Extra.unsignedInt24 endianness)

                0x54 ->
                    Decode.map5 Midi.SMPTEOffset
                        Decode.unsignedInt8
                        Decode.unsignedInt8
                        Decode.unsignedInt8
                        Decode.unsignedInt8
                        Decode.unsignedInt8

                0x58 ->
                    Decode.map4 Midi.TimeSignature
                        Decode.unsignedInt8
                        Decode.unsignedInt8
                        Decode.unsignedInt8
                        Decode.unsignedInt8

                0x59 ->
                    Decode.map2 Midi.KeySignature
                        Decode.signedInt8
                        Decode.unsignedInt8

                0x7F ->
                    Decode.map Midi.SequencerSpecific (Decode.bytes length)

                _ ->
                    Decode.map (Midi.Unknown type_) (Decode.bytes length)
    in
    Decode.unsignedInt8
        |> Decode.andThen
            (\v ->
                variableInt
                    |> Decode.andThen
                        (\v2 ->
                            decodeChunk v2 (decoder v v2)
                        )
            )


systemExclusiveEventType : Decoder Midi.EventType
systemExclusiveEventType =
    variableInt
        |> Decode.andThen
            (\v ->
                Decode.bytes v
            )
        |> Decode.map Midi.SystemExclusive



-- Helpers


variableInt : Decoder Int
variableInt =
    Decode.loop 0
        (\acc ->
            Decode.unsignedInt8
                |> Decode.map
                    (\v ->
                        let
                            next : Int
                            next =
                                v |> Bitwise.and 0x7F |> Bitwise.or acc
                        in
                        if Bitwise.and v 0x80 /= 0 then
                            Decode.Loop (next |> Bitwise.shiftLeftBy 7)

                        else
                            Decode.Done next
                    )
        )


decodeChunk : Int -> Decoder a -> Decoder a
decodeChunk length decoder =
    Decode.bytes length
        |> Decode.andThen
            (\v ->
                case Decode.decode decoder v of
                    Just v2 ->
                        Decode.succeed v2

                    Nothing ->
                        Decode.fail
            )


decodeStringConst : String -> Decoder ()
decodeStringConst a =
    Decode.string (String.length a)
        |> Decode.andThen
            (\v ->
                if v == a then
                    Decode.succeed ()

                else
                    Decode.fail
            )


endianness : Bytes.Endianness
endianness =
    Bytes.BE
