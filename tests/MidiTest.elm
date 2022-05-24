module MidiTest exposing (..)

import Bytes
import Bytes.Encode
import Expect
import Fuzz
import Midi
import Midi.Decode as Decode
import Midi.Encode as Encode
import Test


suite : Test.Test
suite =
    Test.describe "MIDI tests."
        [ Test.fuzz midiFileFuzzer
            "Go to binary and back."
            (\a ->
                Expect.equal
                    (a |> Just)
                    (a |> Encode.file |> Bytes.Encode.encode |> Decode.file)
            )
        ]



--


midiFileFuzzer : Fuzz.Fuzzer Midi.File
midiFileFuzzer =
    Fuzz.map3
        Midi.File
        formatFuzzer
        tempoFuzzer
        tracksFuzzer


tempoFuzzer : Fuzz.Fuzzer Midi.TicksPerBeat
tempoFuzzer =
    Fuzz.map Midi.TicksPerBeat (Fuzz.intRange 1 0x7FFF)


formatFuzzer : Fuzz.Fuzzer Midi.Format
formatFuzzer =
    Fuzz.oneOf
        [ Fuzz.constant Midi.Simultaneous
        , Fuzz.constant Midi.Independent
        ]


tracksFuzzer : Fuzz.Fuzzer ( Midi.Track, List Midi.Track )
tracksFuzzer =
    Fuzz.tuple
        ( trackFuzzer
        , Fuzz.list trackFuzzer
        )


trackFuzzer : Fuzz.Fuzzer Midi.Track
trackFuzzer =
    Fuzz.list eventFuzzer


eventFuzzer : Fuzz.Fuzzer Midi.Event
eventFuzzer =
    Fuzz.map2 Midi.Event ticksFuzzer eventTypeFuzzer


ticksFuzzer : Fuzz.Fuzzer Midi.Ticks
ticksFuzzer =
    Fuzz.map Midi.Ticks (Fuzz.intRange 0 0x0FFFFFFF)


eventTypeFuzzer : Fuzz.Fuzzer Midi.EventType
eventTypeFuzzer =
    Fuzz.oneOf
        [ noteOffFuzzer
        , noteOnFuzzer
        , noteAfterTouchFuzzer
        , controllerChangeFuzzer
        , programChangeFuzzer
        , channelAfterTouchFuzzer
        , pitchBendFuzzer
        , systemExclusiveFuzzer
        ]



--


noteOffFuzzer : Fuzz.Fuzzer Midi.EventType
noteOffFuzzer =
    Fuzz.map3 Midi.NoteOff channelFuzzer noteFuzzer velocityFuzzer


noteOnFuzzer : Fuzz.Fuzzer Midi.EventType
noteOnFuzzer =
    Fuzz.map3 Midi.NoteOn channelFuzzer noteFuzzer positiveVelocityFuzzer


noteAfterTouchFuzzer : Fuzz.Fuzzer Midi.EventType
noteAfterTouchFuzzer =
    Fuzz.map3 Midi.NoteAfterTouch channelFuzzer noteFuzzer velocityFuzzer


controllerChangeFuzzer : Fuzz.Fuzzer Midi.EventType
controllerChangeFuzzer =
    Fuzz.map3 Midi.ControllerChange channelFuzzer controllerNumberFuzzer velocityFuzzer


programChangeFuzzer : Fuzz.Fuzzer Midi.EventType
programChangeFuzzer =
    Fuzz.map2 Midi.ProgramChange channelFuzzer programNumberFuzzer


channelAfterTouchFuzzer : Fuzz.Fuzzer Midi.EventType
channelAfterTouchFuzzer =
    Fuzz.map2 Midi.ChannelAfterTouch channelFuzzer velocityFuzzer


pitchBendFuzzer : Fuzz.Fuzzer Midi.EventType
pitchBendFuzzer =
    Fuzz.map2 Midi.PitchBend channelFuzzer (Fuzz.intRange 0 16383 |> Fuzz.map Midi.Velocity)


systemExclusiveFuzzer : Fuzz.Fuzzer Midi.EventType
systemExclusiveFuzzer =
    Fuzz.map Midi.SystemExclusive bytesFuzzer



--


channelFuzzer : Fuzz.Fuzzer Midi.Channel
channelFuzzer =
    Fuzz.intRange 0 15 |> Fuzz.map Midi.Channel


noteFuzzer : Fuzz.Fuzzer Midi.Note
noteFuzzer =
    Fuzz.intRange 0 127 |> Fuzz.map Midi.Note


velocityFuzzer : Fuzz.Fuzzer Midi.Velocity
velocityFuzzer =
    Fuzz.intRange 0 127 |> Fuzz.map Midi.Velocity


positiveVelocityFuzzer : Fuzz.Fuzzer Midi.Velocity
positiveVelocityFuzzer =
    Fuzz.intRange 1 127 |> Fuzz.map Midi.Velocity


controllerNumberFuzzer : Fuzz.Fuzzer Midi.ControllerNumber
controllerNumberFuzzer =
    Fuzz.intRange 0 119 |> Fuzz.map Midi.ControllerNumber


programNumberFuzzer : Fuzz.Fuzzer Midi.ProgramNumber
programNumberFuzzer =
    Fuzz.intRange 0 127 |> Fuzz.map Midi.ProgramNumber



--


bytesFuzzer : Fuzz.Fuzzer Bytes.Bytes
bytesFuzzer =
    Fuzz.list byteFuzzer |> Fuzz.map (Bytes.Encode.sequence >> Bytes.Encode.encode)


byteFuzzer : Fuzz.Fuzzer Bytes.Encode.Encoder
byteFuzzer =
    Fuzz.intRange 0 255 |> Fuzz.map Bytes.Encode.unsignedInt8
