module Festival.Utils.Translation exposing (..)

import Festival.Locale
import Time


title : String
title =
    "Where to Go Next?"


footer : String
footer =
    "You are welcome!"


locale : Festival.Locale.Locale -> String
locale a =
    case a of
        Festival.Locale.English ->
            "EN"

        Festival.Locale.Czech ->
            "CZ"


date : Time.Zone -> Time.Posix -> String
date zone a =
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


time : Time.Zone -> Time.Posix -> String
time zone a =
    String.padLeft 2 '0' (String.fromInt (Time.toHour zone a))
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
