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


type alias Orientation = EulerRotation

type alias Degrees = Float

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
default = EulerRotation False 0 0 0


{-| Subscribe to changes in orientation for a device.
-}
changes : (Orientation -> msg) -> Sub msg
changes tagger =
    subscription (MySub "deviceorientation" tagger)



-- SUBSCRIPTIONS


type MySub msg
    = MySub String (Orientation -> msg)


subMap : (a -> b) -> MySub a -> MySub b
subMap func (MySub category tagger) =
    MySub category (tagger >> func)



-- EFFECT MANAGER STATE


type alias State msg =
    Dict.Dict String (Watcher msg)


type alias Watcher msg =
    { taggers : List (Orientation -> msg)
    , pid : Process.Id
    }



-- CATEGORIZE SUBSCRIPTIONS


type alias SubDict msg =
    Dict.Dict String (List (Orientation -> msg))


categorize : List (MySub msg) -> SubDict msg
categorize subs =
    categorizeHelp subs Dict.empty


categorizeHelp : List (MySub msg) -> SubDict msg -> SubDict msg
categorizeHelp subs subDict =
    case subs of
        [] ->
            subDict

        (MySub category tagger) :: rest ->
            categorizeHelp rest <|
                Dict.update category (categorizeHelpHelp tagger) subDict


categorizeHelpHelp : a -> Maybe (List a) -> Maybe (List a)
categorizeHelpHelp value maybeValues =
    case maybeValues of
        Nothing ->
            Just [ value ]

        Just values ->
            Just (value :: values)



-- EFFECT MANAGER


init : Task Never (State msg)
init =
    Task.succeed Dict.empty


type alias Msg =
    { category : String
    , orientation : Orientation
    }


(&>) t1 t2 =
    Task.andThen (\_ -> t2) t1


onEffects : Platform.Router msg Msg -> List (MySub msg) -> State msg -> Task Never (State msg)
onEffects router newSubs oldState =
    let
        leftStep category { pid } task =
            Process.kill pid &> task

        bothStep category { pid } taggers task =
            task
                |> Task.andThen (\state -> Task.succeed (Dict.insert category (Watcher taggers pid) state))

        rightStep category taggers task =
            let
                tracker =
                    Dom.onDocument category eulerRotation (Platform.sendToSelf router << Msg category)
            in
                task
                    |> Task.andThen
                        (\state ->
                            Process.spawn tracker
                                |> Task.andThen (\pid -> Task.succeed (Dict.insert category (Watcher taggers pid) state))
                        )
    in
        Dict.merge
            leftStep
            bothStep
            rightStep
            oldState
            (categorize newSubs)
            (Task.succeed Dict.empty)


onSelfMsg : Platform.Router msg Msg -> Msg -> State msg -> Task Never (State msg)
onSelfMsg router { category, orientation } state =
    case Dict.get category state of
        Nothing ->
            Task.succeed state

        Just { taggers } ->
            let
                send tagger =
                    Platform.sendToApp router (tagger orientation)
            in
                Task.sequence (List.map send taggers)
                    &> Task.succeed state
