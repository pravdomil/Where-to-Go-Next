module Festival.Data exposing (..)

import Dict.Any
import Festival.GeoCoordinates
import Festival.Locale
import Id
import Time


type alias Data =
    { events : Dict.Any.Dict (Id.Id Event) Event
    , films : Dict.Any.Dict (Id.Id Film) Film
    , categories : Dict.Any.Dict (Id.Id Category) Category
    , places : Dict.Any.Dict (Id.Id Place) Place
    }



--


type Event
    = Screening_ Screening


eventTime : Event -> Time.Posix
eventTime a =
    case a of
        Screening_ b ->
            b.time


eventDuration : Event -> Int
eventDuration a =
    case a of
        Screening_ b ->
            b.duration



--


type alias Screening =
    { name : Festival.Locale.Localized (Maybe String)
    , type_ : ScreeningType
    , place : Id.Id Place
    , films : List ScreeningFilm
    , time : Time.Posix
    , duration : Int
    }



--


type alias ScreeningFilm =
    { filmId : Id.Id Film
    , note : Festival.Locale.Localized String
    }



--


type ScreeningType
    = Official
    | InvitationOnly



--


type alias Film =
    { name : Festival.Locale.Localized String
    , originalName : String
    , description : Festival.Locale.Localized String
    , images : List String

    --
    , authors : FilmAuthors
    , year : Int
    , duration : Int
    , country : Festival.Locale.Localized String
    , categories : Dict.Any.Dict (Id.Id Category) ()
    , link : Festival.Locale.Localized String
    , imdbLink : String
    , csfdLink : String

    --
    , internalNote : String
    }



--


type alias FilmAuthors =
    { production : List String
    , producer : String
    , directors : List String
    , cast : String
    , dop : String
    , screenplay : String
    , artDirector : String
    , cut : String
    , music : String
    , sound : String
    }



--


type alias Category =
    { name : Festival.Locale.Localized String
    , description : Festival.Locale.Localized String
    , order : Int
    }



--


type alias Place =
    { name : Festival.Locale.Localized String
    , address : String
    , coordinates : Festival.GeoCoordinates.GeoCoordinates
    , code : String
    }
