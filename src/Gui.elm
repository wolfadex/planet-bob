module Gui exposing (button, textField)

import Element exposing (..)
import Element.Border as Border
import Element.Input as Input


button : { label : Element msg, onPress : Maybe msg } -> Element msg
button =
    Input.button
        [ paddingXY 16 8
        , Border.solid
        , Border.width 3
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
