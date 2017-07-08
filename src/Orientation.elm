effect module Orientation
    where { subscription = MySub }
    exposing
        ( Orientation
        , EulerRotation
        , default
        , changes
        )

{-| This library lets you listen to the orientation of the device, if the
device supports
the [experimental device orientation
apis](https://w3c.github.io/deviceorientation/spec-source-orientation.html).

@docs Orientation, EulerRotation, default, changes

-}

import Dict
import Dom.LowLevel as Dom
import Json.Decode as Json
import Process


-- import Quaternion exposing (Quat)

import Task exposing (Task)


-- ORIENTATION


{-| The orientation of the device. Perhaps this will be a quaternion, right now
it's just going to be an alias for EulerRotation
-}
type alias Orientation =
    EulerRotation


type alias Degrees =
    Float


{-| The "euler rotation" of the device, which is just a fancy way of describing
three angles that provide enough information to pinpoint the direction a device
is tilting.

  - **absolute** is whether the frame of reference is the earth or something
    arbitrary.

  - **alpha** is the angle of rotation of the device about the screen. So for
    example when you rotate the device on its side in order to watch a video,
    that's a rotation about the alpha axis. This value will be a float between
    0 and 360 degrees.

  - **beta** is the angle of rotation of the device about horizontal axis.
    So if you've had your phone held flat and you panned it up to take a picture,
    that would be a change in rotation as beta.

  - **gamma** is the angle of rotation of the device about vertical axis.
    So if your device had a camera on the back and you were rotating it while
    taking a panoramic picture, that would be a change in gamma rotation.

These are fairly low-level primitives to have! Wrapping them in Elm makes for
a bit of a tricky situation.

-}
type alias EulerRotation =
    { absolute : Bool
    , alpha : Degrees
    , beta : Degrees
    , gamma : Degrees
    }


{-| The decoder used to extract orientation fields from a deviceorientation event.
-}
eulerRotation : Json.Decoder Orientation
eulerRotation =
    Json.map4 EulerRotation
        (Json.field "absolute" Json.bool)
        (Json.field "alpha" Json.float)
        (Json.field "beta" Json.float)
        (Json.field "gamma" Json.float)


{-| This is a default orientation, useful for representing initial orientation state
-}
default : EulerRotation
default =
    { absolute = False
    , alpha = 0
    , beta = 0
    , gamma = 0
    }


{-| Subscribe to changes in orientation for a device.
-}
changes : (Orientation -> msg) -> Sub msg
changes tagger =
    subscription (MySub tagger)



-- SUBSCRIPTIONS


type MySub msg
    = MySub (Orientation -> msg)


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
    Task.andThen (\_ -> task2) task1


onEffects : Platform.Router msg Orientation -> List (MySub msg) -> State msg -> Task Never (State msg)
onEffects router newSubs oldState =
    case ( oldState, newSubs ) of
        ( Nothing, [] ) ->
            Task.succeed Nothing

        ( Just { pid }, [] ) ->
            Process.kill pid
                &> Task.succeed Nothing

        ( Nothing, _ ) ->
            Process.spawn (Dom.onWindow "deviceorientation" eulerRotation (Task.succeed ()))
                |> Task.andThen (\pid -> Task.succeed (Just { subs = newSubs, pid = pid }))

        ( Just { pid }, _ ) ->
            Task.succeed (Just { subs = newSubs, pid = pid })


onSelfMsg : Platform.Router msg Orientation -> Orientation -> State msg -> Task Never (State msg)
onSelfMsg router dimensions state =
    case state of
        Nothing ->
            Task.succeed state

        Just { subs } ->
            let
                send (MySub tagger) =
                    Platform.sendToApp router (tagger dimensions)
            in
                Task.sequence (List.map send subs)
                    &> Task.succeed state
