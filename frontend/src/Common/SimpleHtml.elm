module Common.SimpleHtml exposing (..)

--import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Monocle.Lens exposing (..)


-- SIMPLE HTML

simpleButton : msg -> String -> Html msg
simpleButton msg title =
    button [ onClick msg ] [ text title ]

simpleInput : (var -> msg) -> Lens var String -> String -> var -> Html msg
simpleInput toMsg lens comment x =
    input [ placeholder comment
          , value (lens.get x)
          , onInput (toMsg << \s -> lens.set s x)
          ]
    []
