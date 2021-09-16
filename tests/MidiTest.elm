module MidiTest exposing (..)

import Char
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Midi
import Random
import Shrink exposing (Shrinker)
import Test exposing (..)


fuzzChannel : Fuzzer Midi.Channel
fuzzChannel =
    Fuzz.intRange 0 15


fuzzNote : Fuzzer Midi.Note
fuzzNote =
    Fuzz.intRange 0 127


fuzzVelocity : Fuzzer Midi.Velocity
fuzzVelocity =
    Fuzz.intRange 0 127


fuzzPositiveVelocity : Fuzzer Midi.Velocity
fuzzPositiveVelocity =
    Fuzz.intRange 1 127


fuzzControllerNumber : Fuzzer Int
fuzzControllerNumber =
    Fuzz.intRange 0 119


generateChannel : Random.Generator Midi.Channel
generateChannel =
    Random.int 0 15


generateNote : Random.Generator Midi.Note
generateNote =
    Random.int 0 127


generateVelocity : Random.Generator Midi.Velocity
generateVelocity =
    Random.int 0 127


generatePositiveVelocity : Random.Generator Midi.Velocity
generatePositiveVelocity =
    Random.int 1 127


generateControllerNumber : Random.Generator Int
generateControllerNumber =
    Random.int 0 119


fuzzNoteOn : Fuzzer Midi.Event
fuzzNoteOn =
    Fuzz.map3 Midi.NoteOn fuzzChannel fuzzNote fuzzPositiveVelocity


fuzzNoteOff : Fuzzer Midi.Event
fuzzNoteOff =
    Fuzz.map3 Midi.NoteOff fuzzChannel fuzzNote fuzzVelocity


fuzzNoteAfterTouch : Fuzzer Midi.Event
fuzzNoteAfterTouch =
    Fuzz.map3 Midi.NoteAfterTouch fuzzChannel fuzzNote fuzzVelocity


fuzzControlChange : Fuzzer Midi.Event
fuzzControlChange =
    Fuzz.map3 Midi.ControlChange fuzzChannel fuzzControllerNumber fuzzVelocity


fuzzProgramChange : Fuzzer Midi.Event
fuzzProgramChange =
    Fuzz.map2 Midi.ProgramChange fuzzChannel fuzzVelocity


fuzzChannelAfterTouch : Fuzzer Midi.Event
fuzzChannelAfterTouch =
    Fuzz.map2 Midi.ChannelAfterTouch fuzzChannel fuzzVelocity


fuzzPitchBend : Fuzzer Midi.Event
fuzzPitchBend =
    Fuzz.map2 Midi.PitchBend fuzzChannel (Fuzz.intRange 0 16383)


generateNoteOn : Random.Generator Midi.Event
generateNoteOn =
    Random.map3 Midi.NoteOn generateChannel generateNote generatePositiveVelocity


generateNoteOff : Random.Generator Midi.Event
generateNoteOff =
    Random.map3 Midi.NoteOff generateChannel generateNote generateVelocity


generateNoteAfterTouch : Random.Generator Midi.Event
generateNoteAfterTouch =
    Random.map3 Midi.NoteAfterTouch generateChannel generateNote generateVelocity


generateControlChange : Random.Generator Midi.Event
generateControlChange =
    Random.map3 Midi.ControlChange generateChannel generateControllerNumber generateVelocity


generateProgramChange : Random.Generator Midi.Event
generateProgramChange =
    Random.map2 Midi.ProgramChange generateChannel generateVelocity


generateChannelAfterTouch : Random.Generator Midi.Event
generateChannelAfterTouch =
    Random.map2 Midi.ChannelAfterTouch generateChannel generateVelocity


generatePitchBend : Random.Generator Midi.Event
generatePitchBend =
    Random.map2 Midi.PitchBend generateChannel (Random.int 0 16383)


fuzzSysExByte : Fuzzer Byte
fuzzSysExByte =
    Fuzz.intRange 0 127


generateSysExByte : Random.Generator Byte
generateSysExByte =
    Random.int 0 127


generateByte : Random.Generator Byte
generateByte =
    Random.int 0 255


listOfLength : Fuzzer a -> Int -> Fuzzer (List a)
listOfLength fuzzer listLen =
    List.foldl
        (Fuzz.map2 (::))
        (Fuzz.constant [])
        (List.repeat listLen fuzzer)


nonEmptyList : Fuzzer a -> Fuzzer (List a)
nonEmptyList fuzzer =
    Fuzz.andThen
        (listOfLength fuzzer)
        (Fuzz.intRange 1 32)


fuzzSysExEvent : Fuzzer Midi.Event
fuzzSysExEvent =
    Fuzz.map
        (\xs -> Midi.SysEx F0 xs)
        (Fuzz.list fuzzSysExByte)


generateSysExFileEvent : Random.Generator Midi.Event
generateSysExFileEvent =
    let
        unescaped : Random.Generator Midi.Event
        unescaped =
            Random.map
                (Midi.SysEx F0)
                (Random.andThen
                    (\nBytes -> Random.list nBytes generateSysExByte)
                    -- NOTE: The parser blows the stack if this is 2048.
                    (Random.int 0 204)
                )

        escaped : Random.Generator Midi.Event
        escaped =
            Random.map
                (Midi.SysEx F7)
                (Random.andThen
                    (\nBytes -> Random.list nBytes generateByte)
                    -- NOTE: The parser blows the stack if this is 2048.
                    (Random.int 0 204)
                )
    in
    Random.choices
        [ unescaped, escaped ]


commonEvents : List (Fuzzer Midi.Event)
commonEvents =
    [ fuzzNoteOn
    , fuzzNoteOff
    , fuzzNoteAfterTouch
    , fuzzControlChange
    , fuzzProgramChange
    , fuzzPitchBend
    ]


commonEventGenerators : List (Random.Generator Midi.Event)
commonEventGenerators =
    [ generateNoteOn
    , generateNoteOff
    , generateNoteAfterTouch
    , generateControlChange
    , generateProgramChange
    , generatePitchBend
    ]


fuzzMidiEvent : Fuzzer Midi.Event
fuzzMidiEvent =
    Fuzz.oneOf <|
        commonEvents
            ++ [ fuzzSysExEvent ]



-- Note: The type is identical to a track, but these are regular MIDI events
-- and not MIDI file events (like in generateTrack).


fuzzMidiEventSequence : Fuzzer (List ( Midi.Ticks, Midi.Event ))
fuzzMidiEventSequence =
    Fuzz.list (Fuzz.map2 (,) (intRange 0 0x0FFFFFFF) fuzzMidiEvent)


generateTrack : Random.Generator Midi.Track
generateTrack =
    Random.andThen
        (\numEvents -> Random.list numEvents generateMidiMessage)
        (Random.frequency
            [ ( 25, Random.constant 0 )
            , ( 50, Random.int 1 8 )
            , ( 24, Random.int 128 256 )
            , ( 1, Random.int 1024 2048 )
            ]
        )


generateMidiFileEvent : Random.Generator Midi.Event
generateMidiFileEvent =
    Random.choices (generateSysExFileEvent :: commonEventGenerators)


generateMidiMessage : Random.Generator MidiMessage
generateMidiMessage =
    Random.pair (Random.int 0 0x0FFFFFFF) generateMidiFileEvent


generateMidiRecording : Random.Generator MidiRecording
generateMidiRecording =
    let
        generateSingleTrack =
            Random.map2
                (\ticks track ->
                    SingleTrack ticks track
                )
                (Random.int 1 0x7FFF)
                generateTrack

        generateMultipleTracks tracksType =
            Random.map2
                (\ticks tracks ->
                    MultipleTracks tracksType ticks tracks
                )
                (Random.int 1 0x7FFF)
                (Random.andThen
                    (\nTracks -> Random.list nTracks generateTrack)
                    (Random.int 0 16)
                )

        generators =
            [ generateSingleTrack
            , generateMultipleTracks Midi.Simultaneous
            , generateMultipleTracks Midi.Independent
            ]
    in
    Random.choices generators


shrinkMidiMessage : Shrinker MidiMessage
shrinkMidiMessage =
    Shrink.noShrink


shrinkMidiTrack : Shrinker Midi.Track
shrinkMidiTrack =
    Shrink.list shrinkMidiMessage


shrinkMidiRecordingSameFormat : Shrinker MidiRecording
shrinkMidiRecordingSameFormat midi =
    case midi of
        SingleTrack ticksPerBeat track ->
            Shrink.map SingleTrack (Shrink.int ticksPerBeat)
                |> Shrink.andMap (shrinkMidiTrack track)

        MultipleTracks tracksType ticksPerBeat tracks ->
            Shrink.map (MultipleTracks tracksType) (Shrink.int ticksPerBeat)
                |> Shrink.andMap (Shrink.list shrinkMidiTrack tracks)


shrinkMidiRecordingChangeFormat : Shrinker MidiRecording
shrinkMidiRecordingChangeFormat midi =
    case midi of
        MultipleTracks tracksType ticksPerBeat ((track :: []) as tracks) ->
            shrinkMidiRecording (SingleTrack ticksPerBeat track)

        _ ->
            Shrink.noShrink midi


shrinkMidiRecording =
    Shrink.merge shrinkMidiRecordingChangeFormat shrinkMidiRecordingSameFormat


fuzzMidiRecording : Fuzzer MidiRecording
fuzzMidiRecording =
    Fuzz.custom
        generateMidiRecording
        shrinkMidiRecording


toByteString : List Int -> String
toByteString list =
    String.fromList (List.map Char.fromCode list)


toFileEvent event =
    case event of
        -- Note we need to add in the EOX byte when storing
        -- sysex messages in a MIDI file.
        Midi.SysEx F0 bytes ->
            Midi.SysEx F0 (bytes ++ [ eox ])

        _ ->
            event


suite : Test
suite =
    describe "MIDI tests"
        [ fuzz fuzzMidiEvent "Go from MidiEvent to \"Binary\" and back" <|
            \event ->
                Expect.equal
                    (Ok event)
                    (Parse.event (toByteString (Generate.event event)))
        , fuzz fuzzMidiRecording "Go from MidiRecording to \"Binary\" and back" <|
            \recording ->
                Expect.equal
                    (Ok recording)
                    (Parse.file (toByteString (Generate.recording recording)))
        , fuzz
            (Fuzz.tuple
                ( fuzzChannel, fuzzNote )
            )
            "NoteOn with velocity zero looks like NoteOff with velocity zero."
          <|
            \( channel, note ) ->
                let
                    noteOn =
                        Midi.NoteOn channel note 0

                    noteOff =
                        Midi.NoteOff channel note 0
                in
                Expect.equal
                    (Parse.event (toByteString (Generate.event noteOn)))
                    (Parse.event (toByteString (Generate.event noteOff)))
        , fuzz fuzzMidiEventSequence "Ensure toFileEvent helper works correctly." <|
            \midiEventSequence ->
                let
                    midiMessages =
                        List.map (\( t, e ) -> ( t, toFileEvent e )) midiEventSequence

                    recording =
                        SingleTrack 1 midiMessages
                in
                Expect.true
                    "Generated recording is valid."
                    (validRecording recording)
        ]
