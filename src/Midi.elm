module Midi exposing
    ( Recording, TrackType(..), Track
    , Message, Ticks, Event(..)
    , Channel, Note, Velocity
    )

{-| Type definitions for MIDI.


# Types

@docs Recording, TrackType, Track

@docs Message, Ticks, Event

@docs Channel, Note, Velocity

-}



{-| MIDI recording.
-}
type alias Recording =
    { type_ : TrackType
    , tempo : TicksPerBeat
    , tracks : ( Track, List Track )
    }


{-| Ticks per beat.
-}
type alias TicksPerBeat =
    Int


{-| Distinguish between track type.
Are they played simultaneously or independently.
-}
type TrackType
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

Note that RunningStatus messages are not included within MidiEvent
because the parser translates them to the underlying channel messages.

-}
type Event
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
    | SysEx (List Int)
    | Unspecified Int (List Int)
      -- channel messages
    | NoteOn Channel Note Velocity
    | NoteOff Channel Note Velocity
    | NoteAfterTouch Channel Note Velocity
    | ControlChange Channel Int Int
    | ProgramChange Channel Int
    | ChannelAfterTouch Channel Velocity
    | PitchBend Channel Int


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
