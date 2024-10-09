module Helpers exposing (cellIdGen)

-- Generate CellId


cellIdGen : number -> number -> number
cellIdGen c r =
    (r * 8) + c
