module PlotAccelerationExample exposing (main)

import Html exposing (Html)
import Html.Attributes exposing (style)
import Svg.Attributes exposing (stroke)
import Task
import Window exposing (Size)
import Device.Motion exposing (Motion)
import Plot exposing (defaultSeriesPlotCustomizations)
import SlidingBuffer exposing (SlidingBuffer)


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
    { history : SlidingBuffer Motion
    , dimensions : Size
    }


init : ( Model, Cmd Msg )
init =
    let
        initialState =
            { history = SlidingBuffer.init 100 Device.Motion.initial
            , dimensions = Size 0 0
            }
    in
        ( initialState, (Task.perform Resize Window.size) )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Move motion ->
            ( { model | history = SlidingBuffer.append model.history motion }, Cmd.none )

        Resize dimensions ->
            ( { model | dimensions = dimensions }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Device.Motion.changes Move
        , Window.resizes Resize
        ]


css =
    """
body {
    margin-right: 0;
    font-family: -apple-system, BlinkMacSystemFont, sans-serif;
}

path {
    mix-blend-mode: multiply;
}
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
                | margin = { top = 15, bottom = 15, left = 50, right = 0 }
                , width = model.dimensions.width
                , height = model.dimensions.height
                , horizontalAxis = Plot.clearAxis
                , toDomainLowest = \y -> y - 0.1
                , toDomainHighest = \y -> y + 0.1
            }

        convertIndexAndMotionToCoordinate index motion =
            { x = toFloat index, acceleration = motion.acceleration }

        coordinates =
            List.indexedMap
                convertIndexAndMotionToCoordinate
                (SlidingBuffer.toList model.history)

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
