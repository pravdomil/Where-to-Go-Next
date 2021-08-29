module Kviff.Translation exposing (..)

import Kviff.Api as Api
import Time


title : String
title =
    "KVIFF Program"


footer : String
footer =
    "You are welcome!"


eventType : Api.EventType -> String
eventType a =
    case a of
        Api.Screening_ ->
            "Screening"

        Api.Event_ ->
            "Event"

        Api.Daily ->
            "Daily"

        Api.Talk ->
            "Talk"

        Api.Exhibition ->
            "Exhibition"

        Api.Restaurant ->
            "Restaurant"


time : Time.Posix -> String
time a =
    let
        zone : Time.Zone
        zone =
            Time.customZone 0 []

        monthToInt : Time.Month -> Int
        monthToInt b =
            case b of
                Time.Jan ->
                    1

                Time.Feb ->
                    2

                Time.Mar ->
                    3

                Time.Apr ->
                    4

                Time.May ->
                    5

                Time.Jun ->
                    6

                Time.Jul ->
                    7

                Time.Aug ->
                    8

                Time.Sep ->
                    9

                Time.Oct ->
                    10

                Time.Nov ->
                    11

                Time.Dec ->
                    12
    in
    String.fromInt (Time.toDay zone a)
        ++ ". "
        ++ String.fromInt (monthToInt (Time.toMonth zone a))
        ++ ". "
        ++ String.padLeft 2 '0' (String.fromInt (Time.toHour zone a))
        ++ ":"
        ++ String.padLeft 2 '0' (String.fromInt (Time.toMinute zone a))


duration : Int -> String
duration a =
    let
        hours : Int
        hours =
            floor (toFloat a / 1000 / 60 / 60)

        minutes : Int
        minutes =
            modBy 60 (floor (toFloat a / 1000 / 60))

        nonZero : String -> Int -> String
        nonZero suffix b =
            if b /= 0 then
                String.fromInt b ++ suffix

            else
                ""
    in
    nonZero "h " hours ++ nonZero "min " minutes
