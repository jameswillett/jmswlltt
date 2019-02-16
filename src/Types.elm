module Types exposing (..)
import Window
import Navigation
import Time exposing (Time)

type Msg
    = NoOp
    | SetScreenSize Window.Size
    | ChangeLocation String
    | UrlChange Navigation.Location
    | ToggleMenuModal
    | RequestTime
    | ReceiveTime Time
    | PhotoModal Int Collection
    | ClearPhotoModal

type ScreenSize
    = Phone
    | Little
    | Big

type alias Collection =
    { title: String
    , id: Int
    , imgs: List String
    }

type Route
    = RootRoute
    | AboutRoute
    | ContactRoute
    | GalleryRoute
    | CollectionRoute Int
    | MusicRoute
    | NotFound
