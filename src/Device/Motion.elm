effect module Device.Motion
    where { subscription = MySub }
    exposing
        ( Motion
        , initial
        , changes
        )

{-| This library lets you listen to the acceleration of the device, if the
device supports
the [experimental device motion
apis](https://w3c.github.io/deviceorientation/spec-source-orientation.html).

@docs EulerRotation, initial, changes

-}

import Dict
import Dom.LowLevel as Dom
import Json.Decode as Json
import Process
import Task exposing (Task)
import Device.Orientation exposing (EulerRotation)


-- ORIENTATION


type alias MetersPerSecondSquared =
    Float


{-| This is a type representing the acceleration of a device.
-}
type alias Acceleration =
    { x : MetersPerSecondSquared
    , y : MetersPerSecondSquared
    , z : MetersPerSecondSquared
    }


{-| This is a type representing the acceleration and
-}
type alias Motion =
    { acceleration : Acceleration
    , accelerationDueToGravity : Acceleration
    , rotationRate : EulerRotation
    , interval : Int
    }


{-| This is a default orientation, useful for representing initial orientation
state.
-}
initial : EulerRotation
initial =
    { alpha = 0
    , beta = 0
    , gamma = 0
    }


{-| Subscribe to changes in orientation for a device.
-}
changes : (Motion -> msg) -> Sub msg
changes tagger =
    subscription (MySub tagger)


{-| The decoder used to extract acceleration info from a motion event.
-}
motion : Json.Decoder motion
motion =
    let
        acceleration =
            Json.field "acceleration" Json.object
    in
        Motion acceleration



-- SUBSCRIPTIONS


type MySub msg
    = MySub (Motion -> msg)


subMap : (a -> b) -> MySub a -> MySub b
subMap func (MySub tagger) =
    MySub (tagger >> func)



-- EFFECT MANAGER


type alias State msg =
    Maybe
        { subs : List (MySub msg)
        , pid : Process.Id
        }


init : Task Never (State msg)
init =
    Task.succeed Nothing


(&>) task1 task2 =
    Task.andThen (always task2) task1


onEffects :
    Platform.Router msg Motion
    -> List (MySub msg)
    -> State msg
    -> Task Never (State msg)
onEffects router newSubs oldState =
    case ( oldState, newSubs ) of
        ( Nothing, [] ) ->
            Task.succeed Nothing

        ( Nothing, _ ) ->
            let
                relayEvent =
                    Platform.sendToSelf router

                eventListener =
                    Dom.onWindow "devicemotion" motion relayEvent

                reportNewManagerState pid =
                    Task.succeed (Just { subs = newSubs, pid = pid })
            in
                Process.spawn eventListener |> Task.andThen reportNewManagerState

        ( Just { pid }, [] ) ->
            Process.kill pid
                &> Task.succeed Nothing

        ( Just { pid }, _ ) ->
            Task.succeed (Just { subs = newSubs, pid = pid })


onSelfMsg :
    Platform.Router msg Motion
    -> Motion
    -> State msg
    -> Task Never (State msg)
onSelfMsg router motion state =
    case state of
        Nothing ->
            Task.succeed state

        Just { subs } ->
            let
                send (MySub tagger) =
                    Platform.sendToApp router (tagger motion)
            in
                Task.sequence (List.map send subs)
                    &> Task.succeed state
