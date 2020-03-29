module NavTabs exposing (navTab)

import Colors
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font


navTab : String -> String -> Bool -> Element msg
navTab txt link_ active =
    let
        ( bgColor, fontColor ) =
            if active then
                ( Colors.secondary, Colors.textOnSecondary )

            else
                ( Colors.white, Colors.secondary )
    in
    link
        [ Border.widthEach { top = 1, right = 1, left = 1, bottom = 1 }
        , paddingXY 24 6
        , Font.size 24
        , Border.color Colors.secondary
        , Background.color bgColor
        , Font.color fontColor
        , Border.roundEach { topLeft = 7, topRight = 7, bottomLeft = 0, bottomRight = 0 }
        ]
        { url = link_
        , label = text txt
        }
