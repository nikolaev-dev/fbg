port module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)


-- model


type alias Model =
    { page : Page
    }


type Page
    = NotFound
    | Start
    | Game
    | Result


initModel : Model
initModel =
    { page = Start }


init : ( Model, Cmd Msg )
init =
    ( initModel, Cmd.none )



-- update


type Msg
    = Navigate Page


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Navigate page ->
            ( { model | page = page }, Cmd.none )



-- view


view : Model -> Html Msg
view model =
    let
        page =
            case model.page of
                Start ->
                    div [ class "row" ]
                        [ div [ class "col-sm-12 col-md-12 col-lg-4" ]
                            [ div [ class "jarviswidget well" ]
                                [ div [ class "widget-body" ]
                                    [ h1 [ class "text-center" ] [ text "First Blockchain Game" ]
                                    , div [ class "text-center" ]
                                        [ br [] []
                                        , br [] []
                                        , a [ class "btn btn-success btn-lg" ] [ text "Start Game" ]
                                        , br [] []
                                        , br [] []
                                        ]
                                    ]
                                ]
                            ]
                        ]

                _ ->
                    div [] []
    in
        div []
            [ page
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
