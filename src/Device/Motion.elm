effect module Device.Motion
    where { subscription = MySub }
    exposing
        ( Motion
        , Acceleration
        , initial
        , changes
        )

{-| This library lets you listen to changes in acceleration of the device
running the Elm program, if the device supports the [experimental device motion
apis](https://w3c.github.io/deviceorientation/spec-source-orientation.html#devicemotion).

@docs Motion, initial, changes

-}

import Dict
import Dom.LowLevel as Dom
import Json.Decode as Json
import Process
import Task exposing (Task)
import Device.Orientation exposing (EulerRotation, eulerRotation)


-- ORIENTATION


type alias MetersPerSecondSquared =
    Float


{-| This is a type representing the acceleration of a device in m/s^2.
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
    , accelerationIncludingGravity : Acceleration
    , rotationRate : EulerRotation
    , interval : Float
    }


{-| This is a default orientation, useful for representing initial orientation
state.
-}
initial : Motion
initial =
    { acceleration = { x = 0, y = 0, z = 0 }
    , accelerationIncludingGravity = { x = 0, y = 0, z = 0 }
    , rotationRate = { alpha = 0, beta = 0, gamma = 0 }
    , interval = 0.0
    }


{-| Subscribe to changes in orientation for a device.
-}
changes : (Motion -> msg) -> Sub msg
changes tagger =
    subscription (MySub tagger)


{-| The decoder used to extract acceleration info from a motion event.
-}
motion : Json.Decoder Motion
motion =
    Json.map4 Motion
        (Json.field "acceleration" accelerationDecoder)
        (Json.field "accelerationIncludingGravity" accelerationDecoder)
        (Json.field "rotationRate" eulerRotation)
        (Json.field "interval" Json.float)


accelerationDecoder : Json.Decoder Acceleration
accelerationDecoder =
    Json.map3 Acceleration
        (Json.field "x" Json.float)
        (Json.field "y" Json.float)
        (Json.field "z" Json.float)



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
