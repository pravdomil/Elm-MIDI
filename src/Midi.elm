module Midi exposing
    ( File, Format(..), Track
    , Event, Ticks(..), EventType(..)
    , Channel(..), Note(..), Velocity(..)
    , ControllerNumber(..), ProgramNumber(..), TicksPerBeat(..)
    )

{-| Type definitions for MIDI.


# Types

@docs File, Format, Track

@docs Event, Ticks, EventType

@docs Channel, Note, Velocity

@docs ControllerNumber, ProgramNumber, TicksPerBeat

-}

import Bytes exposing (Bytes)


{-| A MIDI file.
-}
type alias File =
    { format : Format
    , tempo : TicksPerBeat
    , tracks : ( Track, List Track )
    }


{-| Ticks per beat.
-}
type TicksPerBeat
    = TicksPerBeat Int


{-| Distinguish between MIDI formats.
Are tracks played simultaneously or independently?
-}
type Format
    = Simultaneous
    | Independent


{-| MIDI track.
-}
type alias Track =
    List Event


{-| MIDI event.
-}
type alias Event =
    { delta : Ticks
    , type_ : EventType
    }


{-| MIDI ticks.
-}
type Ticks
    = Ticks Int


{-| MIDI event type.
-}
type EventType
    = -- Meta
      SequenceNumber Int
    | Text String
    | Copyright String
    | TrackName String
    | InstrumentName String
    | Lyrics String
    | Marker String
    | CuePoint String
    | ChannelPrefix Int
    | EndOfTrack
    | Tempo Int
    | SMPTEOffset Int Int Int Int Int
    | TimeSignature Int Int Int Int
    | KeySignature Int Int
    | SequencerSpecific Bytes
    | Unknown Int Bytes
      -- Control
    | NoteOff Channel Note Velocity
    | NoteOn Channel Note Velocity
    | NoteAfterTouch Channel Note Velocity
    | ControllerChange Channel ControllerNumber Velocity
    | ProgramChange Channel ProgramNumber
    | ChannelAfterTouch Channel Velocity
    | PitchBend Channel Velocity
      -- System Exclusive
    | SystemExclusive Bytes


{-| MIDI channel.
-}
type Channel
    = Channel Int


{-| MIDI note.
-}
type Note
    = Note Int


{-| MIDI velocity.
-}
type Velocity
    = Velocity Int


{-| -}
type ControllerNumber
    = ControllerNumber Int


{-| -}
type ProgramNumber
    = ProgramNumber Int
