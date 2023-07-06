module Kviff.Translation exposing (..)

import Kviff.Data
import Time


title : String
title =
    "KVIFF Program"


footer : String
footer =
    "You are welcome!"


eventType : Kviff.Data.EventType -> String
eventType a =
    case a of
        Kviff.Data.Screening_ ->
            "Screening"

        Kviff.Data.Event_ ->
            "Event"

        Kviff.Data.Daily ->
            "Daily"

        Kviff.Data.Talk ->
            "Talk"

        Kviff.Data.Exhibition ->
            "Exhibition"

        Kviff.Data.Restaurant ->
            "Restaurant"


time : Time.Zone -> Time.Posix -> String
time zone a =
    let
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
