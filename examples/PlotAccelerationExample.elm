module PlotAccelerationExample exposing (main)

import Html exposing (Html)
import Html.Attributes exposing (style)
import Svg.Attributes exposing (stroke)
import Task
import Window exposing (Size)
import Device.Motion exposing (Motion)
import Plot exposing (defaultSeriesPlotCustomizations)


main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type Msg
    = Move Motion
    | Resize Size


type alias Model =
    { history : List Motion
    , dimensions : Size
    }


init : ( Model, Cmd Msg )
init =
    let
        initialState =
            { history = List.singleton Device.Motion.initial
            , dimensions = Size 0 0
            }
    in
        ( initialState, (Task.perform Resize Window.size) )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Move motion ->
            ( { model | history = updateMotion model.history motion }, Cmd.none )

        Resize dimensions ->
            ( { model | dimensions = dimensions }, Cmd.none )


updateMotion : List Motion -> Motion -> List Motion
updateMotion history motion =
    motion :: (List.take 75 history)


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Device.Motion.changes Move
        , Window.resizes Resize
        ]


css =
    """
body { font-family: -apple-system, BlinkMacSystemFont, sans-serif; }
path { mix-blend-mode: multiply; }
"""


view : Model -> Html Msg
view model =
    Html.div []
        [ plotMotion model
        , Html.node "style" [] [ Html.text css ]
        ]


plotMotion : Model -> Html Msg
plotMotion model =
    let
        configuration =
            { defaultSeriesPlotCustomizations
                | margin = { top = 50, bottom = 50, left = 80, right = 40 }
                , width = model.dimensions.width
                , height = model.dimensions.height
                , horizontalAxis = Plot.clearAxis
                , toDomainLowest = \y -> y - 0.1
                , toDomainHighest = \y -> y + 0.1
            }

        convertIndexAndMotionToCoordinate index motion =
            { x = toFloat index, acceleration = motion.acceleration }

        coordinates =
            List.indexedMap convertIndexAndMotionToCoordinate model.history

        seriesAlong getter color =
            { axis = Plot.normalAxis
            , interpolation = Plot.Linear (Just color) [ stroke "" ]
            , toDataPoints = List.map (\{ x, acceleration } -> Plot.clear x (getter acceleration))
            }

        series =
            [ seriesAlong .x "cyan"
            , seriesAlong .y "magenta"
            , seriesAlong .z "yellow"
            ]
    in
        Plot.viewSeriesCustom configuration series coordinates


center : Html Msg -> Html Msg
center node =
    let
        styles =
            [ ( "position", "absolute" )
            , ( "top", "0" )
            , ( "bottom", "0" )
            , ( "left", "0" )
            , ( "right", "0" )
            , ( "display", "flex" )
            , ( "align-items", "center" )
            , ( "justify-content", "center" )
            ]
    in
        Html.div [ style styles ] [ node ]
