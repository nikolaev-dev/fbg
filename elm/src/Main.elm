port module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


-- model


type alias Model =
    { state : State
    , moves : List Move
    , my_result : Int
    , partner_result : Int
    }


initModel : Model
initModel =
    { state = StartPage
    , moves = []
    , my_result = 0
    , partner_result = 0
    }


type State
    = StartPage
    | GamePage
    | ResultPage


type alias Move =
    { my_move : Bool
    , partner_move : Bool
    , my_score : Int
    , your_score : Int
    }


init : ( Model, Cmd Msg )
init =
    ( initModel, Cmd.none )



-- update


type Msg
    = Page State
    | Make Move


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Page state ->
            ( { model | state = state }, Cmd.none )

        _ ->
            ( model, Cmd.none )


make : Model -> Move -> ( Model, Cmd Msg )
make model move =
    ( model, Cmd.none )



-- view


view : Model -> Html Msg
view model =
    let
        page =
            case model.state of
                StartPage ->
                    startpage

                GamePage ->
                    gamepage model

                ResultPage ->
                    resultpage model
    in
        div []
            [ page
            ]


gamepage : Model -> Html Msg
gamepage model =
    div [] [ text "Game Page" ]


resultpage : Model -> Html Msg
resultpage model =
    div [] [ text "Result Page" ]


startpage : Html Msg
startpage =
    div [ class "row" ]
        [ div [ class "col-sm-12 col-md-12 col-lg-4" ]
            [ div [ class "jarviswidget well" ]
                [ h1 [ class "text-center" ] [ text "First Blockchain Game" ]
                , div [ class "text-center" ]
                    [ br [] []
                    , br [] []
                    , a [ class "btn btn-success btn-lg", onClick (Page GamePage) ] [ text "Start Game" ]
                    , br [] []
                    , br [] []
                    ]
                ]
            ]
        ]



-- subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
