module Routing exposing (parseLocation)

import UrlParser exposing (..)
import Navigation
import Types exposing (..)

matchers : Parser (Route -> a) a
matchers =
    oneOf
        [ map RootRoute top
        , map AboutRoute (s "about")
        , map ContactRoute (s "contact")
        , map GalleryRoute (s "photo")
        , map CollectionRoute (s "photo" </> int)
        , map MusicRoute (s "music")
        ]

parseLocation : Navigation.Location -> Route
parseLocation location =
    case (parsePath matchers location) of
        Just route ->
            route
        Nothing ->
            NotFound
