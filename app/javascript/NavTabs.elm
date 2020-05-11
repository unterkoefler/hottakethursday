module NavTabs exposing (navTab)

import Colors exposing (ColorScheme)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font


navTab : ColorScheme -> String -> String -> Bool -> Element msg
navTab colorScheme txt link_ active =
    let
        ( bgColor, fontColor ) =
            if active then
                ( colorScheme.secondary, colorScheme.textOnSecondary )

            else
                ( colorScheme.white, colorScheme.secondary )
    in
    link
        [ Border.widthEach { top = 1, right = 1, left = 1, bottom = 1 }
        , paddingXY 24 6
        , Font.size 24
        , Border.color colorScheme.secondary
        , Background.color bgColor
        , Font.color fontColor
        , Border.roundEach { topLeft = 7, topRight = 7, bottomLeft = 0, bottomRight = 0 }
        ]
        { url = link_
        , label = text txt
        }
