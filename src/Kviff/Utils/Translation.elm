module Kviff.Utils.Translation exposing (..)

import Kviff.Locale
import Time


title : String
title =
    "KVIFF Program"


footer : String
footer =
    "You are welcome!"


locale : Kviff.Locale.Locale -> String
locale a =
    case a of
        Kviff.Locale.English ->
            "EN"

        Kviff.Locale.Czech ->
            "CZ"


time : Time.Zone -> Time.Posix -> String
time zone a =
    let
        weekdayToString : Time.Weekday -> String
        weekdayToString b =
            case b of
                Time.Mon ->
                    "Mon"

                Time.Tue ->
                    "Tue"

                Time.Wed ->
                    "Wed"

                Time.Thu ->
                    "Thu"

                Time.Fri ->
                    "Fri"

                Time.Sat ->
                    "Sat"

                Time.Sun ->
                    "Sun"
    in
    weekdayToString (Time.toWeekday zone a)
        ++ " "
        ++ String.fromInt (Time.toDay zone a)
        ++ " - "
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
