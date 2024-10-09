module Helpers exposing (cuidGen)


cuidGen : number -> number -> number
cuidGen c r =
    (r * 8) + c
