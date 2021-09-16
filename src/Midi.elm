module Midi exposing
    ( Recording(..), TrackType(..), Track
    , Ticks, Event(..)
    , Channel, Note, Velocity, SysExFlavour(..)
    , endOfExclusive, isValidRecording
    , Message
    )

{-| Type definitions for MIDI.


# Types

@docs Recording, TrackType, Track

@docs MidiMessage, Ticks, Event

@docs Channel, Note, Velocity, SysExFlavour


# Helpers

@docs endOfExclusive, isValidRecording

-}


{-| Midi recording.
-}
type Recording
    = Recording Int Track (List Track)


{-| Distinguish between track type.
Are they played simultaneously or independently.
-}
type TrackType
    = Simultaneous
    | Independent


{-| Midi track.
-}
type alias Track =
    List Message



--


{-| Midi message.
-}
type alias Message =
    ( Ticks, Event )


{-| Midi tick (elapsed time).
-}
type alias Ticks =
    Int


{-| Midi event.

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

Distinguish between two different forms of system messages as characterised by the lead-in byte.

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
Verifies multipart system messages.
-}
isValidRecording : Recording -> Bool
isValidRecording (Recording _ a b) =
    let
        validTrack : Bool -> List Message -> Bool
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
    List.all (validTrack False) (a :: b)
