module Midi exposing
    ( File, Format(..), Track
    , Message, Ticks, Event(..)
    , Channel, Note, Velocity
    )

{-| Type definitions for MIDI.


# Types

@docs File, Format, Track

@docs Message, Ticks, Event

@docs Channel, Note, Velocity

-}

import Bytes exposing (Bytes)


{-| A MIDI file.
-}
type alias File =
    { tempo : TicksPerBeat
    , format : Format
    , tracks : ( Track, List Track )
    }


{-| Ticks per beat.
-}
type alias TicksPerBeat =
    Int


{-| Distinguish between MIDI formats.
Are tracks played simultaneously or independently?
-}
type Format
    = Simultaneous
    | Independent


{-| MIDI track.
-}
type alias Track =
    List Message



--


{-| MIDI message.
-}
type alias Message =
    ( Ticks, Event )


{-| MIDI tick (elapsed time).
-}
type alias Ticks =
    Int


{-| MIDI event.
-}
type Event
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
    | Tempo Int
    | SMPTEOffset Int Int Int Int Int
    | TimeSignature Int Int Int Int
    | KeySignature Int Int
    | SequencerSpecific (List Int)
    | SysEx Bytes
    | Unspecified Int (List Int)
      -- Control
    | NoteOn Channel Note Velocity
    | NoteOff Channel Note Velocity
    | NoteAfterTouch Channel Note Velocity
    | ControlChange Channel Int Int
    | ProgramChange Channel Int
    | ChannelAfterTouch Channel Velocity
    | PitchBend Channel Int
      -- Other
    | EndOfTrack


{-| MIDI channel.
-}
type alias Channel =
    Int


{-| MIDI note.
-}
type alias Note =
    Int


{-| MIDI velocity (volume).
-}
type alias Velocity =
    Int
