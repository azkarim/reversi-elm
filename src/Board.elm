module Board exposing (..)

import Array
import Direction exposing (..)
import Helpers exposing (cellIdGen)
import List.Extra as ListE


type alias Column =
    Int


type alias Row =
    Int


type alias Position =
    { col : Int
    , row : Int
    }


type alias CellId =
    Int


type alias Cell =
    ( CellId, Position, Status )


type Status
    = Empty
    | Playable Player
    | Played Potent Player


type Player
    = Home
    | Away


type Potent
    = Fill
    | None


type Direction
    = North
    | South
    | East
    | West
    | NorthEast
    | NorthWest
    | SouthWest
    | SouthEast


opponent : Player -> Player
opponent turn =
    case turn of
        Home ->
            Away

        Away ->
            Home


flattenArr : Array.Array (Array.Array Cell) -> List Cell
flattenArr arr =
    arr
        -- A(A Cell)
        |> Array.toList
        -- L(A Cell)
        >> List.map (\el -> Array.toList el)
        -- L(L Cell)
        >> List.concat



-- Extract all the Empty Cells and filter the ones that can be played and tag them as Playable by Player.


findEmpty : Array.Array (Array.Array Cell) -> Player -> List Cell
findEmpty board turn =
    let
        flatArr =
            flattenArr board

        north_ n =
            n - 8

        northEast_ n =
            n - 7

        east_ n =
            n + 1

        southEast_ n =
            n + 9

        south_ n =
            n + 8

        southWest_ n =
            n + 7

        west_ n =
            n - 1

        northWest_ n =
            n - 9

        dirList pos =
            if (pos.col == 0) && (pos.row == 0) then
                [ east_, south_, southEast_ ]

            else if (pos.col == 7) && (pos.row == 0) then
                [ west_, south_, southWest_ ]

            else if (pos.col == 7) && (pos.row == 7) then
                [ north_, northWest_, west_ ]

            else if (pos.col == 0) && (pos.row == 7) then
                [ north_, northEast_, east_ ]

            else if (pos.col == 0) && (pos.row >= 1) && (pos.row <= 6) then
                [ north_, northEast_, east_, southEast_, south_ ]

            else if (pos.col == 7) && (pos.row >= 1) && (pos.row <= 6) then
                [ north_, northWest_, west_, southWest_, south_ ]

            else if (pos.row == 7) && (pos.col >= 1) && (pos.col <= 6) then
                [ north_, northWest_, west_, east_ ]

            else if (pos.row == 0) && (pos.col >= 1) && (pos.col <= 6) then
                [ east_, southEast_, south_, southWest_, west_ ]

            else
                [ north_, northEast_, east_, southEast_, south_, southWest_, west_, northWest_ ]

        surroundingCells =
            flatArr
                |> List.filter (\( _, _, s ) -> (s == Played Fill (opponent turn)) || (s == Played None (opponent turn)))
                |> List.map (\( c, pos, _ ) -> List.map (\dir -> dir c) (dirList pos))
                |> List.concat
    in
    flatArr
        |> List.filter (\( cuid, _, stat ) -> List.member cuid surroundingCells && (stat == Empty))


ifEmptyIsReducible : Array.Array (Array.Array Cell) -> Player -> List Cell -> List Cell
ifEmptyIsReducible board turn list =
    list
        |> List.map
            (\cell ->
                if List.isEmpty (reduceBoard board turn cell) then
                    []

                else
                    [ cell ]
            )
        |> List.concat


turnToPlayable : Player -> List Cell -> List Cell
turnToPlayable turn list =
    list
        |> List.map (\( c, p, s ) -> ( c, p, Playable turn ))



--- If a Cell can be played


reduceBoard : Array.Array (Array.Array Cell) -> Player -> Cell -> List Cell
reduceBoard board turn ( cuid, pos, status ) =
    succRedList board turn South ( cuid, pos, status )
        |> (++) (succRedList board turn North ( cuid, pos, status ))
        |> (++) (succRedList board turn East ( cuid, pos, status ))
        |> (++) (succRedList board turn West ( cuid, pos, status ))
        |> (++) (succRedList board turn NorthEast ( cuid, pos, status ))
        |> (++) (succRedList board turn NorthWest ( cuid, pos, status ))
        |> (++) (succRedList board turn SouthWest ( cuid, pos, status ))
        |> (++) (succRedList board turn SouthEast ( cuid, pos, status ))


succRedList : Array.Array (Array.Array Cell) -> Player -> Direction -> Cell -> List Cell
succRedList board turn dir ( cuid, pos, status ) =
    let
        isRedPossible =
            possibleReducList board turn dir ( cuid, pos, status )
                |> checkOffsetItem board turn dir
    in
    if isRedPossible then
        possibleReducList board turn dir ( cuid, pos, status )

    else
        []


checkOffsetItem : Array.Array (Array.Array Cell) -> Player -> Direction -> List Cell -> Bool
checkOffsetItem board turn dir list =
    let
        offsetCuid ( c, p, s ) =
            case dir of
                North ->
                    cellIdGen p.col (p.row - 1)

                South ->
                    cellIdGen p.col (p.row + 1)

                East ->
                    if p.col == 7 then
                        cellIdGen p.col -2

                    else
                        cellIdGen (p.col + 1) p.row

                West ->
                    if p.col == 0 then
                        cellIdGen p.col -2

                    else
                        cellIdGen (p.col - 1) p.row

                NorthEast ->
                    if p.col == 7 then
                        cellIdGen p.col -2

                    else
                        cellIdGen (p.col + 1) (p.row - 1)

                NorthWest ->
                    if p.col == 0 then
                        cellIdGen p.col -2

                    else
                        cellIdGen (p.col - 1) (p.row - 1)

                SouthWest ->
                    if p.col == 0 then
                        cellIdGen p.col -2

                    else
                        cellIdGen (p.col - 1) (p.row + 1)

                SouthEast ->
                    if p.col == 7 then
                        cellIdGen p.col -2

                    else
                        cellIdGen (p.col + 1) (p.row + 1)

        lastVal =
            case ListE.last list of
                Just a ->
                    a

                Nothing ->
                    ( 100, Position 100 100, Empty )

        retrievedLastStat =
            flattenArr board
                |> List.filter (\( cuid, pos, stat ) -> cuid == offsetCuid lastVal)
                |> List.map (\( _, _, stat ) -> stat)
                |> List.head
                |> Maybe.withDefault Empty
    in
    (retrievedLastStat == Played Fill turn) || (retrievedLastStat == Played None turn)


possibleReducList : Array.Array (Array.Array Cell) -> Player -> Direction -> Cell -> List Cell
possibleReducList board turn dir ( cuid, pos, status ) =
    let
        reducList =
            case dir of
                North ->
                    north ( cuid, pos, status )

                South ->
                    south ( cuid, pos, status )
                        |> List.sort
                        |> List.reverse

                East ->
                    east ( cuid, pos, status )

                West ->
                    west ( cuid, pos, status )

                NorthEast ->
                    northEast ( cuid, pos, status )

                NorthWest ->
                    northWest ( cuid, pos, status )

                SouthWest ->
                    southWest ( cuid, pos, status )

                SouthEast ->
                    southEast ( cuid, pos, status )

        sortDir dir_ list =
            case dir_ of
                North ->
                    list
                        |> List.sortBy (\( cuid0, _, _ ) -> cuid0)
                        |> List.reverse

                South ->
                    list
                        |> List.sortBy (\( cuid0, _, _ ) -> cuid0)

                East ->
                    list
                        |> List.sortBy (\( cuid0, _, _ ) -> cuid0)

                West ->
                    list
                        |> List.sortBy (\( cuid0, _, _ ) -> cuid0)
                        |> List.reverse

                NorthEast ->
                    list
                        |> List.sortBy (\( cuid0, _, _ ) -> cuid0)
                        |> List.reverse

                NorthWest ->
                    list
                        |> List.sortBy (\( cuid0, _, _ ) -> cuid0)
                        |> List.reverse

                SouthWest ->
                    list
                        |> List.sortBy (\( cuid0, _, _ ) -> cuid0)

                SouthEast ->
                    list
                        |> List.sortBy (\( cuid0, _, _ ) -> cuid0)
    in
    flattenArr board
        |> List.filter (\( cuid0, _, _ ) -> List.member cuid0 reducList)
        -- Sort is direction dependent
        |> sortDir dir
        |> ListE.takeWhile (\( _, _, stat ) -> (stat /= Empty) && (stat /= Playable turn) && (stat /= Playable (opponent turn)))
        |> ListE.takeWhile (\( _, _, stat ) -> (stat /= Played Fill turn) && (stat /= Played None turn))
