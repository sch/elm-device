effect module Device.Orientation
    where { subscription = MySub }
    exposing
        ( EulerRotation
        , eulerRotation
        , initial
        , changes
        )

{-| This library lets you listen to the rotational orientation of the device,
so long as it supports the [experimental device orientation
apis](https://w3c.github.io/deviceorientation/spec-source-orientation.html).

@docs EulerRotation, eulerRotation, initial, changes

-}

import Dict
import Dom.LowLevel as Dom
import Json.Decode as Json
import Process
import Task exposing (Task)


-- ORIENTATION


type alias Degrees =
    Float


{-| The "euler rotation" of the device, which is just a fancy way of describing
three angles that provide enough information to pinpoint the direction a device
is tilting.

  - **alpha** is the angle of rotation of the device about the screen. So for
    example, if you're looking at your phone on a table, and you spin it around
    like a game of spin-the-bottle, the alpha example points in the direction of
    the person you must kiss. This value will be a float between 0 and 360
    degrees.

  - **beta** is the angle of rotation of the device about horizontal axis.
    So if you've had your phone held flat and you panned it up to take a picture,
    that would be a change in rotation as beta. This value is a float between
    -180 and 180 degrees.

  - **gamma** is the angle of rotation of the device about vertical axis.
    So if your device had a camera on the back and you were rotating it while
    taking a panoramic picture, that would be a change in gamma rotation. This
    value is a float between -90 and 90 degrees.

These are fairly low-level primitives to have! The use of EulerRotations can be
convenient, as they report useful values about the rotation about the screen
itself.

-}
type alias EulerRotation =
    { alpha : Degrees
    , beta : Degrees
    , gamma : Degrees
    }


{-| This is the starting orientation, useful for representing initial orientation
state in a model.
-}
initial : EulerRotation
initial =
    { alpha = 0
    , beta = 0
    , gamma = 0
    }


{-| Subscribe to changes in orientation for a device.
-}
changes : (EulerRotation -> msg) -> Sub msg
changes tagger =
    subscription (MySub tagger)


{-| The decoder used to extract orientation fields from a deviceorientation event.
-}
eulerRotation : Json.Decoder EulerRotation
eulerRotation =
    Json.map3 EulerRotation
        (Json.field "alpha" Json.float)
        (Json.field "beta" Json.float)
        (Json.field "gamma" Json.float)



-- SUBSCRIPTIONS


type MySub msg
    = MySub (EulerRotation -> msg)


subMap : (a -> b) -> MySub a -> MySub b
subMap func (MySub tagger) =
    MySub (tagger >> func)



-- EFFECT MANAGER


{-| Our state here is the state of the part of this module that keeps track of
what's attached to what. There may be an event listener registered and some
number of subscriptions interested in that event, or there may be nothing at
all.
-}
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


{-| onEffects is a function called by the Elm runtime when your app's code
makes requests of the outside world. It's called to perform work when there's
a transition between two points in time. Here are the cases handled:

  - When the manager has **no state and no subscriptions**, do nothing and return
    nothing! We don't have anyone listening, and we haven't been listening.

  - When the manager receives **a new subscription, but isn't yet listening**,
    then register an event listener. This event listener runs in a lightweight
    process, as a task that never completes. This process has a process ID,
    a unique identifier that allows the runtime to keep track of the process.
    We take this id and stick it in the effect manager's state alongside the
    newly requested subscription. The event data, after being converted with
    the eulerRotation decoder, is then passed back to the platform so that the
    onSelfMsg handler can figure out what to do with it.

  - When the manager receives **a new subscription, and is already listening for
    orientation events**, then we just add the new subscription to our existing
    subscriptions. We don't need to do anything else, since there's already an
    event listener process going.

  - When the app has **existing manager state, but no subscribers**, then stop
    listening! We take that process id and kill it, which uses `Dom.onWindow` to
    unregister the event listener. We then clear the effect manager state,
    since there's nothing in our app interested in orientation events anymore.

-}
onEffects :
    Platform.Router msg EulerRotation
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
                    Dom.onWindow "deviceorientation" eulerRotation relayEvent

                reportNewManagerState pid =
                    Task.succeed (Just { subs = newSubs, pid = pid })
            in
                Process.spawn eventListener |> Task.andThen reportNewManagerState

        ( Just { pid }, [] ) ->
            Process.kill pid
                &> Task.succeed Nothing

        ( Just { pid }, _ ) ->
            Task.succeed (Just { subs = newSubs, pid = pid })


{-| onSelfMsg is a function called by the Elm runtime when an effect manager
has something to report. We handle two cases when there's an orientation
effect:

  - If there aren't any subscribers, do nothing!

  - If there are subscribers, report the `EulerRotation` value to them

-}
onSelfMsg :
    Platform.Router msg EulerRotation
    -> EulerRotation
    -> State msg
    -> Task Never (State msg)
onSelfMsg router rotation state =
    case state of
        Nothing ->
            Task.succeed state

        Just { subs } ->
            let
                send (MySub tagger) =
                    Platform.sendToApp router (tagger rotation)
            in
                Task.sequence (List.map send subs)
                    &> Task.succeed state
