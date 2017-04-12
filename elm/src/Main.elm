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
    , partner_score : Int
    }


type alias Choise =
    { my : Bool
    , partner : Bool
    }


type alias Points =
    { my : Int
    , partner : Int
    }


init : ( Model, Cmd Msg )
init =
    ( initModel, Cmd.none )



-- update


type Msg
    = Page State
    | Make Bool


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Page state ->
            ( { model | state = state }, Cmd.none )

        Make bool ->
            make model bool


make : Model -> Bool -> ( Model, Cmd Msg )
make model choise =
    let
        lastMove =
            List.head model.moves

        partnerNewMove =
            case lastMove of
                Nothing ->
                    True

                Just move ->
                    move.my_move

        newPoints =
            scores { my = choise, partner = partnerNewMove }

        newMove =
            Move choise
                partnerNewMove
                newPoints.my
                newPoints.partner

        newMoves =
            newMove :: model.moves
    in
        ( { model | moves = newMoves }, Cmd.none )


scores : Choise -> Points
scores choise =
    case [ choise.my, choise.partner ] of
        [ True, True ] ->
            { my = 100, partner = 100 }

        [ True, False ] ->
            { my = -50, partner = 150 }

        [ False, True ] ->
            { my = 150, partner = -50 }

        _ ->
            { my = -10, partner = -10 }



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
    div []
        [ h1 [ class "text-center" ] [ text "First Blockchain Game" ]
        , h2 [ class "text-center" ] [ text "Create Your Choise" ]
        , choiseBlock
        , scoreBlock model
        ]


scoreBlock : Model -> Html Msg
scoreBlock model =
    section []
        [ div [ class "row" ]
            [ article [ class "col-sm-12 col-md-12 col-lg-4" ]
                [ div [ class "jarviswidget" ]
                    [ header []
                        [ span [ class "widget-icon" ]
                            [ i [ class "fa fa-table" ] [] ]
                        , h2 [] [ text "Game Result" ]
                        ]
                    , div []
                        [ div [ class "widget-body no-padding" ]
                            [ div [ class "table-responsive" ]
                                [ scoreBlockTable model
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]


scoreBlockTable : Model -> Html a
scoreBlockTable model =
    table [ class "table" ]
        [ thead []
            [ tr []
                [ tr [] [ text "Me" ]
                , tr [] [ text "Partner" ]
                , tr [] [ text "My score" ]
                , tr [] [ text "Partner score" ]
                ]
            ]
        , tbody []
            (List.map moveTr model.moves)
        ]


moveTr : Move -> Html a
moveTr move =
    tr []
        [ td [] [ text (boolToStr move.my_move) ]
        , td [] [ text (boolToStr move.partner_move) ]
        , td [] [ text (toString move.my_score) ]
        , td [] [ text (toString move.partner_score) ]
        ]


boolToStr : Bool -> String
boolToStr bool =
    case bool of
        True ->
            "+"

        False ->
            "-"


choiseBlock : Html Msg
choiseBlock =
    div [ class "row" ]
        [ div [ class "col-xs-6 text-center" ]
            [ a [ class "btn btn-danger btn-circle btn-xl" ]
                [ i [ class "glyphicon glyphicon-thumbs-down", onClick (Make False) ] []
                ]
            ]
        , div [ class "col-xs-6 text-center" ]
            [ a [ class "btn btn-success btn-circle btn-xl" ]
                [ i [ class "glyphicon glyphicon-thumbs-up", onClick (Make True) ] []
                ]
            ]
        ]


resultpage : Model -> Html Msg
resultpage model =
    div [] [ text "Result Page" ]


startpage : Html Msg
startpage =
    div []
        [ h1 [ class "text-center" ] [ text "First Blockchain Game" ]
        , div [ class "text-center" ]
            [ br [] []
            , br [] []
            , a [ class "btn btn-success btn-lg", onClick (Page GamePage) ] [ text "Start Game" ]
            , br [] []
            , br [] []
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
