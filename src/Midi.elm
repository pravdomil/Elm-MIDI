module Midi exposing
    ( MidiRecording(..), TracksType(..), Track
    , MidiMessage, Ticks, MidiEvent(..)
    , Note, Velocity, SysExFlavour(..)
    , Channel, endOfExclusive, isValidRecording
    )

{-| Type definitions for MIDI.


# Data Types

@docs Track, MidiEvent, MidiMessage, TracksType, MidiRecording, Byte, Channel

@docs Note, Velocity, SysExFlavour, Ticks


# Helpers

@docs eox, validRecording

-}


{-| Midi Recording
-}
type MidiRecording
    = SingleTrack Int Track
    | MultipleTracks TracksType Int (List Track)


{-| Discriminate between the types of tracks in a recording.
Are they played simultaneously or independently.
-}
type TracksType
    = Simultaneous
    | Independent


{-| Midi Track
-}
type alias Track =
    List MidiMessage



--


{-| Midi Message
-}
type alias MidiMessage =
    ( Ticks, MidiEvent )


{-| Midi tick (elapsed time).
-}
type alias Ticks =
    Int


{-| Midi Event

Note that RunningStatus messages are not included within MidiEvent
because the parser translates them to the underlying channel messages

-}
type MidiEvent
    = -- meta messages
      SequenceNumber Int
    | Text String
    | Copyright String
    | TrackName String
    | InstrumentName String
    | Lyrics String
    | Marker String
    | CuePoint String
    | ChannelPrefix Int
    | Tempo Int
    | SMPTEOffset Int Int Int Int Int
    | TimeSignature Int Int Int Int
    | KeySignature Int Int
    | SequencerSpecific (List Int)
    | SysEx SysExFlavour (List Int)
    | Unspecified Int (List Int)
      -- channel messages
    | NoteOn Channel Note Velocity
    | NoteOff Channel Note Velocity
    | NoteAfterTouch Channel Note Velocity
    | ControlChange Channel Int Int
    | ProgramChange Channel Int
    | ChannelAfterTouch Channel Velocity
    | PitchBend Channel Int


{-| Midi channel.
-}
type alias Channel =
    Int


{-| Midi note.
-}
type alias Note =
    Int


{-| Midi velocity (volume).
-}
type alias Velocity =
    Int


{-| System exclusive message.

Discriminate between two different forms of SysExEvrnt as characterised by the
lead-in byte, See <https://www.csie.ntu.edu.tw/~r92092/ref/midi/#sysex_event>.

-}
type SysExFlavour
    = F0 -- normal
    | F7 -- escaped


{-|


# Helpers

-}


{-| End of exclusive byte.
-}
endOfExclusive : Int
endOfExclusive =
    0xF7


{-| Checks if MidiRecording is valid.
Verifies multipart sysex messages.
-}
isValidRecording : MidiRecording -> Bool
isValidRecording a =
    let
        validTrack : Bool -> List MidiMessage -> Bool
        validTrack multipart b =
            case b of
                -- All multipart messages must be finished.
                [] ->
                    not multipart

                t :: ts ->
                    case ( t, multipart ) of
                        ( ( _, SysEx F0 data ), False ) ->
                            case List.head (List.reverse data) of
                                Just eox ->
                                    validTrack False ts

                                _ ->
                                    validTrack True ts

                        -- After the first packet all parts of a multipart packet
                        -- start with F7.
                        ( ( _, SysEx F0 _ ), True ) ->
                            False

                        ( ( _, SysEx F7 data ), True ) ->
                            case List.head (List.reverse data) of
                                Just eox ->
                                    validTrack False ts

                                _ ->
                                    validTrack True ts

                        ( ( _, SysEx F7 _ ), False ) ->
                            validTrack multipart ts

                        -- There must be no MIDI events in between the packets of a
                        -- multipart sysex message.
                        ( _, True ) ->
                            False

                        _ ->
                            validTrack multipart ts
    in
    case a of
        SingleTrack _ b ->
            validTrack False b

        MultipleTracks _ _ b ->
            List.all (validTrack False) b
