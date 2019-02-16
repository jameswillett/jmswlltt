module NavTo exposing (navTo)

import Types exposing (..)
import Html exposing (Html, Attribute, a)
import Html.Attributes exposing (href)
import Html.Events exposing (onWithOptions)
import Json.Decode as Decode

navTo : String -> List (Attribute Msg) -> List (Html Msg) -> Html Msg
navTo url attributes children =
    let
        att = (href url) :: (onLinkClick (ChangeLocation url)) :: attributes
    in
        a att children

onLinkClick : msg -> Attribute msg
onLinkClick message =
    let
        options =
            { stopPropagation = False
            , preventDefault = True
            }
    in
        onWithOptions
            "click"
            options
            (preventDefaultUnlessKeyPressed
                |> Decode.andThen (maybePreventDefault message)
            )

preventDefaultUnlessKeyPressed : Decode.Decoder Bool
preventDefaultUnlessKeyPressed =
    Decode.map2
        nor
        (Decode.field "ctrlKey" Decode.bool)
        (Decode.field "metaKey" Decode.bool)

nor : Bool -> Bool -> Bool
nor x y =
    not (x || y)

maybePreventDefault : msg -> Bool -> Decode.Decoder msg
maybePreventDefault msg preventDefault =
    case preventDefault of
        True ->
            Decode.succeed msg

        False ->
            Decode.fail "Delegated to browser default"
