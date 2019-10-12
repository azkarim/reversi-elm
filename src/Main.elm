module Main exposing (..)

import Board exposing (..)
import Browser exposing (element)
import Html.Styled as H exposing (Html, toUnstyled, div, button)
import Html.Styled.Events as HE exposing (onClick)
import Html.Styled.Attributes as HA exposing (style)
import Svg.Styled exposing (..)
import Svg.Styled.Attributes as SA exposing (..)
import Svg.Styled.Events exposing (onClick, onMouseOver, onMouseOut)
import Css.Global exposing (Snippet, global)
import Style exposing (..)
import Array
import List.Extra as ListE
import Set
import Task
import Process
import Random.Extra as RandomE
import Random


main : Program () Model Msg
main =
    element
        { init = init
        , update = update
        , view = view >> H.toUnstyled
        , subscriptions = \_ -> Sub.none
        }


view : Model -> Html Msg
view model =
    div []
        [ styleStore
        , viewGame model
        ]


viewGame : Model -> Html Msg
viewGame model =
    case model.view of
        Welcome ->
            div [] [ button [ HE.onClick StartGame ] [ text "Start Game" ] ]

        Game ->
            viewGame_ model



-- CSS


styleStore : Html Msg
styleStore =
    global <| [ reset ] ++ mainStyle



--- Model


init : () -> ( Model, Cmd Msg )
init _ =
    ( initModel Welcome Home
    , randomPlayer PickPlayer
    )


initModel : View -> Player -> Model
initModel view_ player =
    Model initBoard player view_ (Position 100 100) Playing


randomPlayer : (Player -> Msg) -> Cmd Msg
randomPlayer msg =
    RandomE.choice Home Away
        |> Random.generate msg


type alias Model =
    { board : Array.Array (Array.Array Cell)
    , turn : Player
    , view : View
    , highlight : Position
    , gameStatus : GameStatus
    }



--- Types


type View
    = Welcome
    | Game


type GameStatus
    = Playing
    | Pass Player
    | GameOver



--- Update


type Msg
    = Play Cell
    | PickPlayer Player
    | PickPlayerAgain Player
    | StartGame
    | ShowPotent Cell
    | RemovePotent Cell
    | PassTurn
    | EndGame
    | Restart


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        showPlayable board turn =
            findEmpty board turn
                |> ifEmptyIsReducible board turn
                |> turnToPlayable turn
    in
        case msg of
            PickPlayer player ->
                ( { model | turn = player }, Cmd.none )

            Restart ->
                ( { model | gameStatus = Playing }, randomPlayer PickPlayerAgain )

            PickPlayerAgain player ->
                ( { model | turn = player, board = showPlayable initBoard player |> patchCellList initBoard }, Cmd.none )

            PassTurn ->
                let
                    advancePlayable =
                        showPlayable model.board (opponent model.turn)
                            |> patchCellList model.board
                            |> flattenArr
                            |> List.filter (\( _, _, s ) -> s == (Playable (opponent model.turn)))

                    isPass =
                        if List.isEmpty advancePlayable then
                            Pass (opponent model.turn)
                        else
                            Playing
                in
                    if not (List.isEmpty advancePlayable) then
                        ( { model | gameStatus = Playing, turn = opponent model.turn, board = showPlayable model.board (opponent model.turn) |> patchCellList model.board }, Cmd.none )
                    else
                        ( { model | turn = opponent model.turn, gameStatus = Pass (opponent model.turn) }
                        , Process.sleep 1500 |> Task.perform (always EndGame)
                        )

            EndGame ->
                ( { model | gameStatus = GameOver }, Cmd.none )

            StartGame ->
                ( { model | board = (showPlayable model.board model.turn |> patchCellList model.board), view = Game }
                , Cmd.none
                )

            Play ( cuid, { col, row }, stat ) ->
                let
                    findEmptyOrPlayable =
                        flattenArr model.board
                            |> List.filter (\( _, _, s ) -> (s == Empty) || (s == Playable Home) || (s == Playable Away))

                    updatedBoard =
                        updateBoard model ( cuid, Position col row, stat )

                    opponentPlayable =
                        flattenArr updatedBoard
                            |> List.filter (\( _, _, s ) -> (s == Playable (opponent model.turn)))

                    advancePlayable =
                        updatedBoard
                            |> (\board -> showPlayable board model.turn)

                    isPass =
                        if (List.isEmpty opponentPlayable) && (List.isEmpty advancePlayable) then
                            GameOver
                        else if List.isEmpty opponentPlayable then
                            Pass (opponent model.turn)
                        else
                            Playing
                in
                    ( { model | board = updatedBoard, gameStatus = isPass, turn = opponent model.turn }
                    , (if (List.isEmpty opponentPlayable) && (List.isEmpty advancePlayable) then
                        Cmd.none
                       else if List.isEmpty opponentPlayable then
                        Process.sleep 1500 |> Task.perform (always PassTurn)
                       else
                        Cmd.none
                      )
                    )

            ShowPotent ( cuid, { col, row }, stat ) ->
                ( { model | board = showPotent model ( cuid, Position col row, stat ), highlight = Position col row }
                , Cmd.none
                )

            RemovePotent ( cuid, { col, row }, stat ) ->
                ( { model | board = removePotent model ( cuid, Position col row, stat ), highlight = Position 100 100 }
                , Cmd.none
                )


showPotent { board, turn } ( cuid, pos, stat ) =
    reduceBoard board turn ( cuid, pos, stat )
        |> List.map (\( c, p, s ) -> ( c, p, Played Fill (opponent turn) ))
        |> patchCellList board


removePotent { board, turn } ( cuid, pos, stat ) =
    let
        revertPotent s0 =
            case s0 of
                Played Fill pl ->
                    Played None pl

                _ ->
                    Empty
    in
        flattenArr board
            |> List.filter
                (\( c, p, s ) ->
                    if ((s == Played Fill Home) || (s == Played Fill Away)) then
                        True
                    else
                        False
                )
            |> List.map (\( c, p, s ) -> ( c, p, revertPotent s ))
            |> patchCellList board


updateBoard { board, turn } ( cuid, pos, stat ) =
    let
        overturnedList =
            reduceBoard board turn ( cuid, pos, stat )
                |> List.map (\( c, p, s ) -> ( c, p, Played None turn ))
                >> (\list ->
                        if List.isEmpty list then
                            []
                        else
                            (++) [ ( cuid, pos, Played None turn ) ] list
                   )

        removePlayableStat =
            flattenArr board
                |> List.filter (\( _, _, s ) -> s == (Playable turn))
                |> List.filter (\( c, _, _ ) -> c /= cuid)
                |> List.map (\( c, p, _ ) -> ( c, p, Empty ))

        advanceTurn =
            case turn of
                Home ->
                    Away

                Away ->
                    Home

        newBoard =
            (++) overturnedList removePlayableStat
                |> patchCellList board
    in
        findEmpty newBoard advanceTurn
            |> ifEmptyIsReducible newBoard advanceTurn
            |> turnToPlayable advanceTurn
            |> patchCellList newBoard



--- Util


patchCellList : Array.Array (Array.Array Cell) -> List Cell -> Array.Array (Array.Array Cell)
patchCellList board list =
    let
        extractCuid =
            List.map (\( cuid0, _, _ ) -> cuid0) list
    in
        flattenArr board
            |> List.filter (\( cuid1, _, _ ) -> not (List.member cuid1 extractCuid))
            >> (++) list
            >> createBoard


createBoard : List Cell -> Array.Array (Array.Array Cell)
createBoard list =
    let
        sortedList =
            List.sortBy (\( cuid0, _, _ ) -> cuid0) list

        splitList begin end =
            List.take end sortedList
                |> List.drop begin
    in
        List.range 1 8
            |> List.map (\n -> splitList (8 * (n - 1)) (n * 8))
            >> List.map (\x -> Array.fromList x)
            >> Array.fromList


flattenArr : Array.Array (Array.Array Cell) -> List Cell
flattenArr arr =
    arr
        -- A(A Cell)
        |> Array.toList
        -- L(A Cell)
        >> List.map (\el -> Array.toList el)
        -- L(L Cell)
        >> List.concat



--- View


initPatch =
    [ ( 27, Position 3 3, Played None Home ), ( 35, Position 3 4, Played None Away ), ( 28, Position 4 3, Played None Away ), ( 36, Position 4 4, Played None Home ) ]


initBoard =
    let
        cuidConv =
            (\c r -> (r * 8) + c)

        col =
            List.range 0 7

        colGen row =
            List.map (\col_ -> ( (cuidConv col_ row), Position col_ row, Empty )) col
                |> Array.fromList
    in
        List.range 0 7
            |> List.map (\row -> colGen row)
            >> Array.fromList
            >> (\b -> patchCellList b initPatch)


viewGrid : Model -> List (Svg Msg)
viewGrid { board, highlight } =
    let
        arrToList_ list =
            list |> Array.toList
    in
        board
            |> Array.toList
            >> List.map (\list -> arrToList_ list)
            >> List.concat
            >> List.map (\cell -> viewACell highlight cell)


viewACell : Position -> Cell -> Svg Msg
viewACell highlight ( cuid, { col, row }, status ) =
    let
        x_ =
            col * 60

        y_ =
            row * 60

        viewCircle =
            let
                circle_ color stroke_ =
                    circle
                        [ cx <| String.fromInt (x_ + 30)
                        , cy <| String.fromInt (y_ + 30)
                        , r "21"
                        , fill <| color
                        , stroke <| stroke_
                        , strokeWidth "3"
                        , disableOnClick status
                        , disableCursor status
                        , disableOnMouseOver status
                        , disableOnMouseOut status
                        ]
                        []
            in
                case status of
                    Empty ->
                        text ""

                    Playable Home ->
                        circle_ "#70ae6e" "#000"

                    Playable Away ->
                        circle_ "#70ae6e" "#fff"

                    Played None Home ->
                        circle_ "#000" "#000"

                    Played None Away ->
                        circle_ "#fff" "#fff"

                    Played Fill Home ->
                        circle_ "#f6f6f6" "#000"

                    Played Fill Away ->
                        circle_ "#222" "#fff"

        disableOnMouseOver stat =
            case stat of
                Playable _ ->
                    onMouseOver (ShowPotent ( cuid, Position col row, status ))

                _ ->
                    SA.style ""

        disableOnMouseOut stat =
            case stat of
                Playable _ ->
                    onMouseOut (RemovePotent ( cuid, Position col row, status ))

                _ ->
                    SA.style ""

        disableOnClick stat =
            case stat of
                Played _ _ ->
                    SA.style ""

                Empty ->
                    SA.style ""

                _ ->
                    onClick (Play ( cuid, Position col row, status ))

        disableCursor stat =
            case stat of
                Playable _ ->
                    SA.cursor "pointer"

                _ ->
                    SA.cursor ""

        viewBg h =
            if ((Position h.col h.row) == (Position col row)) then
                "#65a863"
            else
                "#70ae6e"
    in
        g []
            [ rect
                [ width "60"
                , height "60"
                , x <| String.fromInt x_
                , y <| String.fromInt y_
                , fill <| viewBg highlight
                , stroke "#243e23"
                , disableOnClick status
                , disableCursor status
                ]
                []
            , viewCircle
            ]


viewScore : Model -> Player -> Int
viewScore model player =
    flattenArr model.board
        |> List.filter (\( _, _, s ) -> (s == Played Fill player) || (s == Played None player))
        |> List.length


showPassOrScore : Model -> Player -> String
showPassOrScore model player =
    if (model.gameStatus == Pass player) then
        "Pass"
    else
        String.fromInt <| viewScore model player


showBorder : Model -> Player -> String
showBorder model player =
    if (model.gameStatus == Playing) && (model.turn == player) then
        " border"
    else
        ""


viewButton : GameStatus -> String
viewButton model =
    case model of
        GameOver ->
            "Play Again"

        _ ->
            "Restart"


type PanelOrResult
    = Panel
    | GameResult


viewWinnerOrTie : Model -> PanelOrResult -> Player -> String
viewWinnerOrTie model panelResult player =
    let
        flatBoard =
            flattenArr model.board

        playerScore =
            viewScore model player

        opponentScore =
            viewScore model (opponent player)
    in
        case panelResult of
            GameResult ->
                if (model.gameStatus == GameOver) && (playerScore == opponentScore) then
                    "Tie"
                else if (model.gameStatus == GameOver) && (playerScore > opponentScore) then
                    "Winner"
                else
                    "Loser"

            Panel ->
                if (model.gameStatus == GameOver) && (playerScore == opponentScore) then
                    " show"
                else if (model.gameStatus == GameOver) && (playerScore > opponentScore) then
                    " show"
                else
                    ""


viewGame_ : Model -> Html Msg
viewGame_ model =
    div []
        [ div []
            [ div [ class "game" ]
                [ div [ class "statContainer" ]
                    [ div [ class <| (++) "circleContainer" <| showBorder model Home ]
                        [ div [ class "circle" ] [ text <| showPassOrScore model Home ]
                        ]
                    , div [ class <| (++) "resultContainer" <| viewWinnerOrTie model Panel Home ]
                        [ div [ class "result" ] [ text <| viewWinnerOrTie model GameResult Home ]
                        ]
                    ]
                , viewSvg model
                , div [ class "statContainer" ]
                    [ div [ class <| (++) "circleContainer" <| showBorder model Away ]
                        [ div [ class "circle opponent" ] [ text <| showPassOrScore model Away ]
                        ]
                    , div [ class <| (++) "resultContainer" <| viewWinnerOrTie model Panel Away ]
                        [ div [ class "result" ] [ text <| viewWinnerOrTie model GameResult Away ] ]
                    ]
                ]
            , div [ class "restart" ] [ button [ HE.onClick Restart ] [ text <| viewButton model.gameStatus ] ]
            ]
        ]


viewSvg : Model -> Html Msg
viewSvg model =
    svg
        [ width "480"
        , height "480"
        , viewBox "0 0 480 480"
        , SA.style "border : 1px solid #222"
        ]
        (viewGrid model)
