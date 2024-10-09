module Direction exposing (east, north, northEast, northWest, south, southEast, southWest, west)

import Helpers exposing (cellIdGen)



--- CORE FUNCTIONS


northEast : ( number, { a | row : number, col : number }, c ) -> List number
northEast ( cuid, pos, _ ) =
    if (pos.row >= 2) && (pos.row <= 7) && (pos.col >= 0) && (pos.col <= 5) then
        let
            recurse n list =
                let
                    boundary =
                        if (pos.row == 7) && (pos.col >= 1) && (pos.col <= 5) then
                            cellIdGen pos.row pos.col

                        else if (pos.row == 6) && (pos.col >= 2) && (pos.col <= 5) then
                            cellIdGen 7 (pos.col - 1)

                        else if (pos.row == 5) && (pos.col >= 3) && (pos.col <= 5) then
                            cellIdGen 7 (pos.col - 2)

                        else if (pos.row == 4) && (pos.col >= 4) && (pos.col <= 5) then
                            cellIdGen 7 (pos.col - 3)

                        else if (pos.row == 3) && (pos.col == 5) then
                            cellIdGen 7 (pos.col - 4)

                        else
                            1
                in
                if n > 1 then
                    recurse (n - 7) (n - 7 :: list)

                else
                    list
                        |> List.filter (\x -> x >= boundary)
        in
        recurse cuid []

    else
        []


northWest : ( number, { a | row : number, col : number }, c ) -> List number
northWest ( cuid, pos, _ ) =
    if (pos.row >= 2) && (pos.row <= 7) && (pos.col >= 2) && (pos.col <= 7) then
        let
            recurse n list =
                let
                    boundary =
                        if (pos.col == 2) && (pos.row >= 4) && (pos.row <= 7) then
                            cellIdGen 0 (pos.row - 2)

                        else if (pos.col == 3) && (pos.row >= 5) && (pos.row <= 7) then
                            cellIdGen 0 (pos.row - 3)

                        else if (pos.col == 4) && (pos.row >= 6) && (pos.row <= 7) then
                            cellIdGen 0 (pos.row - 4)

                        else if (pos.col == 5) && (pos.row == 7) then
                            cellIdGen 0 (pos.row - 5)

                        else
                            0
                in
                if n >= 0 then
                    recurse (n - 9) (n - 9 :: list)

                else
                    list
                        |> List.filter (\x -> x >= boundary)
        in
        recurse cuid []

    else
        []


southEast : ( number, { a | col : number, row : number }, c ) -> List number
southEast ( cuid, pos, _ ) =
    if (pos.col >= 0) && (pos.col <= 5) && (pos.row >= 0) && (pos.row <= 5) then
        let
            recurse n list =
                let
                    boundary =
                        if (pos.col == 5) && (pos.row >= 0) && (pos.row <= 3) then
                            cellIdGen 7 (pos.row + 2)

                        else if (pos.col == 4) && (pos.row >= 0) && (pos.row <= 2) then
                            cellIdGen 7 (pos.row + 3)

                        else if (pos.col == 3) && (pos.row >= 0) && (pos.row <= 1) then
                            cellIdGen 7 (pos.row + 4)

                        else if (pos.col == 2) && (pos.row == 0) then
                            cellIdGen 7 (pos.row + 5)

                        else
                            69
                in
                if n <= 69 then
                    recurse (n + 9) (n + 9 :: list)

                else
                    list
                        |> List.filter (\x -> x <= boundary)
        in
        recurse cuid []

    else
        []


southWest : ( number, { a | col : number, row : number }, c ) -> List number
southWest ( cuid, pos, _ ) =
    if (pos.col >= 2) && (pos.col <= 7) && (pos.row >= 0) && (pos.row <= 5) then
        let
            recurse n list =
                let
                    boundary =
                        if (pos.row == 0) && (pos.col >= 2) && (pos.col <= 6) then
                            cellIdGen 0 pos.col

                        else if (pos.row == 1) && (pos.col >= 2) && (pos.col <= 5) then
                            cellIdGen 0 (pos.col + 1)

                        else if (pos.row == 2) && (pos.col >= 2) && (pos.col <= 4) then
                            cellIdGen 0 (pos.col + 2)

                        else if (pos.row == 3) && (pos.col >= 2) && (pos.col <= 3) then
                            cellIdGen 0 (pos.col + 3)

                        else if (pos.row == 4) && (pos.col == 2) then
                            cellIdGen 0 (pos.col + 4)

                        else
                            8 * 7 + 5
                in
                if n <= (8 * 7 + 5) then
                    recurse (n + 7) (n + 7 :: list)

                else
                    list
                        |> List.filter (\x -> x <= boundary)
        in
        recurse cuid []

    else
        []


north : ( number, { a | row : number }, c ) -> List number
north ( cuid, pos, _ ) =
    if pos.row >= 2 then
        let
            recurse n list =
                if n >= 0 then
                    recurse (n - 8) (n - 8 :: list)

                else
                    list
                        |> List.filter (\x -> x >= 0)
        in
        recurse cuid []

    else
        []


south : ( number, { a | row : number }, c ) -> List number
south ( cuid, pos, _ ) =
    if pos.row <= 5 then
        let
            recurse n list =
                if (n >= 0) && (n <= 63) then
                    recurse (n + 8) (n + 8 :: list)

                else
                    list
                        |> List.filter (\x -> x <= 63)
        in
        recurse cuid []

    else
        []


east : ( number, { a | col : number, row : number }, c ) -> List number
east ( cuid, pos, _ ) =
    if pos.col <= 5 then
        let
            recurse n list =
                if n <= cellIdGen 7 pos.row then
                    recurse (n + 1) (n + 1 :: list)

                else
                    list
                        |> List.filter (\x -> x <= cellIdGen 7 pos.row)
        in
        recurse cuid []

    else
        []


west : ( number, { a | col : number, row : number }, c ) -> List number
west ( cuid, pos, _ ) =
    if pos.col >= 2 then
        let
            recurse n list =
                if n >= cellIdGen 0 pos.row then
                    recurse (n - 1) (n - 1 :: list)

                else
                    list
                        |> List.filter (\x -> x >= cellIdGen 0 pos.row)
        in
        recurse cuid []

    else
        []
