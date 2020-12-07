module Gui exposing (button, textField, card)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input


button : { label : Element msg, onPress : Maybe msg } -> Element msg
button =
    Input.button
        [ paddingXY 16 8
        , Border.solid
        , Border.widthEach
            { top = 2
            , bottom = 4
            , left = 4
            , right = 2
            }
        , mouseOver
            [ Border.color (rgb 0.4 0.9 0.9)
            , Background.color (rgba 0 0 0 0.7)
            , Font.color (rgb 0.4 0.9 0.9)
            ]
        ]


textField : { label : String, value : String, onChange : String -> msg } -> Element msg
textField { label, value, onChange } =
    Input.text
        []
        { onChange = onChange
        , placeholder = Nothing
        , label =
            label
                |> text
                |> Input.labelLeft []
        , text = value
        }


card : List (Attribute msg) -> Element msg -> Element msg
card additionalAttributes =
    el
        ([ spacing 8
         , padding 8
         , Border.solid
         , Border.widthEach
            { top = 2
            , bottom = 4
            , left = 4
            , right = 2
            }
         ]
            ++ additionalAttributes
        )
