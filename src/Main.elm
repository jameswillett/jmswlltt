module Main exposing (..)

import Html exposing (Html, Attribute, a, text, div, h1, h2, img, span, ul, li, p)
import Html.Attributes exposing (src, width, class, style, href)
import Html.Events exposing (onClick)
import Window exposing (resizes)
import Routing exposing (parseLocation)
import Types exposing (..)
import Navigation
import NavTo exposing (navTo)
import Time exposing (Time)
import Task

---- MODEL ----

type alias Model =
    { screenSize: ScreenSize
    , modal: Bool
    , path: String
    , route: Route
    , time: Maybe Time
    , selectedCollection: Maybe Collection
    , collectionIndex: Int
    }

type alias Flags =
    { width : Int
    -- , height : Int
    }

initialModel : Flags -> Route -> Model
initialModel f l =
    { modal = False
    , screenSize = getScreenSize <| Window.Size f.width 0
    , path = ""
    , route = l
    , time = Nothing
    , selectedCollection = Nothing
    , collectionIndex = 0
    }

init : Flags -> Navigation.Location -> ( Model, Cmd Msg )
init f l =
    let
        currentRoute = parseLocation l
    in
        ( initialModel f currentRoute) |> update ( UrlChange l )

---- UPDATE ----

getScreenSize : Window.Size -> ScreenSize
getScreenSize size =
    if size.width < 600 then
        Phone
    else if size.width <= 1200 then
        Little
    else
        Big

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlChange newLocation ->
            let
                newRoute = parseLocation newLocation
            in
                ({ model |
                      path = newLocation.pathname
                      , route = newRoute
                 }, Task.perform ReceiveTime Time.now)
        ChangeLocation path ->
            ({ model | modal = False }, Navigation.newUrl path)
        SetScreenSize size ->
            ({ model | screenSize = getScreenSize size, modal = False }, Cmd.none)
        ToggleMenuModal ->
            ({ model | modal = not model.modal }, Cmd.none)
        RequestTime ->
            (model, Task.perform ReceiveTime Time.now)
        ReceiveTime time ->
            ({ model | time = Just time }, Cmd.none)
        PhotoModal i c ->
            ({ model |
                   selectedCollection = Just c
                   , collectionIndex = i
                   }, Cmd.none)
        ClearPhotoModal ->
            ({ model | selectedCollection = Nothing }, Cmd.none)
        NoOp ->
            (model, Cmd.none)

---- VIEW ----

mobileHeader : Model -> Html Msg
mobileHeader model =
    div [ class "mobile-header" ]
        [ div [ class "mobile-header-name"] [ h2 [] [ text "james willett" ]]
        , div [ class "headerbutton"]
              [ span [ class "h1button"
                     , onClick ToggleMenuModal ]
                     [ text "menu" ]
              ]
        ]

links : List (Html Msg)
links =
    [ navTo "/about" [] [ text "about" ]
    , navTo "/photo" [] [ text "photography" ]
    , navTo "/music" [] [ text "music" ]
    , navTo "/contact" [] [ text "contact" ]
    ]

header : Model -> Html Msg
header model =
    if model.path == "/" then span [] []
    else if model.screenSize == Phone then mobileHeader model
    else
        let
            w = 100 // List.length links |> (\x -> (toString x) ++ "%")
            linkStyle = style [ ("width", w) ]
        in
            div [ class "header" ]
                (h1 [] [ text "JAMES WILLETT" ] ::
                    List.map (\x -> div [ linkStyle, class "header-links" ] [x]) links)

modal : Model -> Html Msg
modal model =
    if model.screenSize /= Phone || not model.modal
    then span [ class "empty-span" ] []
    else
        div [ class "modal" ]
            [ div [ class "modalbutton"
                  , onClick ToggleMenuModal
                  ]
                  [ span [] [ text "close"] ]
            , div [ class "modalcontent" ]
                  [ List.map (\x -> li [ class "modal-link" ] [x]) links
                        |> ul [ class "modal-list" ] ]
            ]
first : List String -> String
first l =
    let
        m = List.head l
    in
        case m of
            Just x -> x
            Nothing -> ""

randImg : Model -> List String -> String
randImg m l =
    let
        time =
            case m.time of
                Just x -> x
                Nothing -> 0
        len = List.length l
        item = (floor time) % len
    in
        first (List.drop item l)

makeImgList : String -> Int -> List String
makeImgList head len =
    let
        indecies = List.map toString <| List.range 1 len
    in
        List.map (\x -> "https://s3.amazonaws.com/jmswllttphotos/" ++ head ++ "-" ++ x) indecies

collections : List Collection
collections =
    [ { title = "La Casa 2/19/2016"
      , id = 1
      , imgs = makeImgList "lacasa-2-19-2016" 7
      }
    , { title = "Dougout 5/18/2016"
      , id = 2
      , imgs = makeImgList "dougout-5-18-2016" 11
      }
    , { title = "538 Johnson 5/21/2016"
      , id = 3
      , imgs = makeImgList "538-5-21-2016" 16
      }
    , { title = "Despise You 5/21/2017"
      , id = 4
      , imgs = makeImgList "despiseyou" 18
      }
    , { title = "Puerto Rico '16 & '17"
      , id = 5
      , imgs = makeImgList "puertorico" 22
      }
    , { title = "Coke Bust Finnish Excursion"
      , id = 6
      , imgs = makeImgList "cokebust-finland" 49
      }
    , { title = "Messlife"
      , id = 7
      , imgs = makeImgList "messlife" 33
      }
    , { title = "Family"
      , id = 8
      , imgs = makeImgList "family" 25
      }
    , { title = "Damaged City 2016"
      , id = 9
      , imgs = makeImgList "damagedcity2016" 71
      }
    , { title = "The Pinch 5/15/2016"
      , id = 10
      , imgs = makeImgList "pinch-5-15-2016" 17
      }
    , { title = "Coke Bust Japan 2015"
      , id = 11
      , imgs = makeImgList "cokebust-japan" 55
      }
    , { title = "DCCX 2015"
      , id = 12
      , imgs = makeImgList "cross-is-coming" 21
      }
    ]


u : String -> String
u s =
    "url(" ++ s ++ ".jpg)"

thumb : String -> String
thumb s =
  s ++ "-thumb" |> u -- literally everywhere we use `u`
                     -- EXCEPT the main modal, use this
                     -- once we have thumbs uploaded
                     -- then think about renaming `u` because
                     -- that name fucking sucks

responsiveWidth : Model -> Attribute msg
responsiveWidth model =
    let
        (width, margin) =
            case model.screenSize of
                Phone -> ("100%", "0%")
                Little -> ("31%", "1%")
                Big -> ("23%", "1%")

    in
        style [ ("width", width)
              , ("margin", margin)
              ]

bgStyle : String -> Attribute msg
bgStyle url =
    style
        [ ("backgroundImage", u url)
        , ("backgroundSize", "cover")
        , ("background-position", "center")
        ]

collection : Model -> String -> String -> Int-> Html Msg
collection m b t id =
    let
        con = "collection-container"
        p = "collection-parent"
        bg = "collection-bg"
        c = "collection-child"
        url = "/photo/" ++ toString id
    in
        div [ responsiveWidth m, class p ]
            [ navTo url [ class "link" ]
                        [ div [ bgStyle b, class bg ]
                              [ div [ class c ] [ text t ]]
                        ]
            ]

responsiveImg : Model -> Attribute Msg
responsiveImg model =
    let
        width =
            case model.screenSize of
                Phone -> "49.7vw"
                Little -> "19.6vw"
                Big -> "14.1vw"
    in
        style [ ("width", width)
              , ("height", width)
              , ("display", "inline-block")
              ]

imageCellBg : String -> Attribute Msg
imageCellBg url =
    style [ ("backgroundImage", url |> u)
          , ("backgroundSize", "cover")
          , ("backgroundRepeat", "no-repeat")
          , ("background-position", "center")
          ]

getCollection : Int -> Maybe Collection
getCollection id =
    List.filter (\x -> x.id == id) collections |> List.head

collectionView : Model -> Collection -> Html Msg
collectionView model c =
    div []
        [ case model.selectedCollection of
            Just c -> photoModal model c
            Nothing ->
             div []
                  ([ div [ class "collectionTitle" ] [ text c.title ]]
                      ++ List.indexedMap
                          (\i url ->
                              div [responsiveImg model
                                  , class "imageCellParent"
                                  , onClick (PhotoModal i c)
                                  ]
                                  [ div [ imageCellBg url
                                       , class "imageCell"
                                        ] []
                                  ]
                          ) c.imgs
                  )
        ]

photoModal : Model -> Collection -> Html Msg
photoModal m c =
    let
        display =
            case m.selectedCollection of
                Nothing -> "none"
                Just c -> "block"
        s = style [ ("display", display)
                  , ("position", "fixed")
                  , ("top", "0")
                  , ("left", "0")
                  , ("height", "100%")
                  , ("width", "100%")
                  , ("backgroundColor", "rgba(0,0,0,1)")
                  -- , ("zIndex", "1")
                  ]
        url =
            List.drop m.collectionIndex c.imgs |> first
        imgStyle =
            style [ ("backgroundImage", url |> u)
                  , ("backgroundSize", "contain")
                  , ("backgroundRepeat","no-repeat")
                  , ("background-position","center top")
                  , ("max-width", "1000px")
                  , ("max-height", "667px")
                  , ("height", "100%")]
        prev =
            if m.collectionIndex == 0
            then 0
            else m.collectionIndex - 1

        prevStyle =
            let
                display =
                    if m.collectionIndex == 0
                    then "none"
                    else "inline-block"
                url =
                    List.drop (m.collectionIndex - 1) c.imgs |> first
            in
                style [("display", display)
                      , ("backgroundImage", url |> u)
                      , ("backgroundSize", "contain")
                      , ("backgroundRepeat", "no-repeat")]
        next =
            if m.collectionIndex == (List.length c.imgs) - 1
            then m.collectionIndex
            else m.collectionIndex + 1

        nextStyle =
            let
                display =
                    if m.collectionIndex == (List.length c.imgs) - 1
                    then "none"
                    else "inline-block"
                url =
                    List.drop (m.collectionIndex + 1) c.imgs |> first
            in
                style [ ("display", display)
                      , ("backgroundImage", url |> u)
                      , ("backgroundSize", "contain")
                      , ("backgroundRepeat", "no-repeat")]
    in
        div [ s, class "photomodal" ]
            [ div [ class "modalbutton"
                  , onClick ClearPhotoModal ]
                  [ text "GO BACK"]
            , div [ class "modalimg", imgStyle ] []
            , div [ class "modalNavParentL" ]
                  [ div [ class "prevImg"
                        , prevStyle
                        , onClick <| PhotoModal prev c ]
                        [ text "prev" ]
                  ]
            , div [ class "modalNavParentR" ]
                  [ div [ class "nextImg"
                        , nextStyle
                        , onClick <| PhotoModal next c ]
                        [ text "next" ]
                  ]
            ]

photos : Model -> Html Msg
photos model =
    let
        d_ = collection model
        bg = randImg model
    in
        div []
            <| List.map
                (\c -> d_ (bg c.imgs) c.title c.id) 
                collections

splash : Html Msg
splash =
    div [ class "splash" ]
        [ p [] [ text "hi!" ]
        , text "->"
        , navTo "/about" [ class "link" ] [ text "click to enter" ]
        , text "<-"
        ]

router : Model -> Html Msg
router model =
    case model.route of
        RootRoute -> splash
        AboutRoute -> span [] [ text "james willett is cool" ]
        ContactRoute -> span [] [ text "maybe a link to my github or something will go here" ]
        CollectionRoute id ->
            case getCollection id of
                Just x -> collectionView model x
                Nothing -> span [] [ text "something went wrong" ]
        GalleryRoute -> photos model
        MusicRoute -> span [] [ text "i make music" ]
        NotFound -> span [] [ text "not found" ]

view : Model -> Html Msg
view model =
    let
        content m = div [ class "content" ] [ router m ]
    in
        div []
            [ modal model
            , header model
            , content model
            ]

---- SUBZ ----

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [
        resizes (\size ->
            if getScreenSize size == model.screenSize
            then NoOp
            else SetScreenSize size
        )
    ]

---- ROUTING ----

---- PROGRAM ----

main : Program Flags Model Msg
main =
    Navigation.programWithFlags UrlChange
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
