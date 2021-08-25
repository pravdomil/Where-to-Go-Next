module Kviff.Api exposing (..)

import Json.Decode as D
import Utils.Json.Decode_ as D_


type alias Event =
    { id : Int
    , type_ : String
    , typeName : Localized String

    --
    , name : Localized String
    , description : Localized String

    --
    , timeStart : Maybe String
    , timeEnd : Maybe String

    --
    , place : Place

    --
    , order : Int
    }


type alias Place =
    { id : Int
    , name : Localized String

    --
    , address : String
    , gps : String

    --
    , phone : String
    , website : String
    }



--


type Locale
    = English
    | Czech


type alias Localized a =
    { en : a
    , cz : a
    }



--


decodeEvent : D.Decoder Event
decodeEvent =
    D.map8
        (\v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17 v18 ->
            { id = v3
            , type_ = v16
            , typeName = Localized v18 v17

            --
            , name = Localized v12 v11
            , description = Localized v15 v14

            --
            , timeStart = v2
            , timeEnd = v1

            --
            , place =
                { id = v8
                , name = Localized v6 v5

                --
                , address = v4
                , gps = v7

                --
                , phone = v9
                , website = v10
                }

            --
            , order = v13
            }
        )
        (D.field "cas_do" (D_.maybe D.string))
        (D.field "cas_od" (D_.maybe D.string))
        (D.field "id" D.int)
        (D.field "misto_adresa" D.string)
        (D.field "misto_cz" D.string)
        (D.field "misto_en" D.string)
        (D.field "misto_gps" D.string)
        (D.field "misto_id" D.int)
        |> D_.apply (D.field "misto_telefon" D.string)
        |> D_.apply (D.field "misto_website" D.string)
        |> D_.apply (D.field "nazev_cz" D.string)
        |> D_.apply (D.field "nazev_en" D.string)
        |> D_.apply (D.field "order" D.int)
        |> D_.apply (D.field "popis_cz" D.string)
        |> D_.apply (D.field "popis_en" D.string)
        |> D_.apply (D.field "typ" D.string)
        |> D_.apply (D.field "typ_cz" D.string)
        |> D_.apply (D.field "typ_en" D.string)
