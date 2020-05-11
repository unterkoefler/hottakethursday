module Colors exposing (ColorScheme, colorSchemeForUser)

import Data.User exposing (User)
import Element exposing (rgb)


white =
    rgb 1.0 1.0 1.0


black =
    rgb 0.0 0.0 0.0


type alias ColorScheme =
    { primary : Element.Color
    , primaryDark : Element.Color
    , secondary : Element.Color
    , secondaryLight : Element.Color
    , textOnPrimary : Element.Color
    , textOnSecondary : Element.Color
    , link : Element.Color
    , white : Element.Color
    , black : Element.Color
    , gray : Element.Color
    , lightGray : Element.Color
    }


defaultColorScheme : ColorScheme
defaultColorScheme =
    { -- red
      primary =
        rgb 0.917647 0.0 0.070588
    , -- red
      primaryDark =
        rgb 0.68627 0.0 0.0
    , -- dark red
      secondary =
        rgb 1.0 0.47843 0.0
    , -- orange
      secondaryLight =
        rgb 1.0 0.670588 0.25882
    , textOnPrimary =
        white
    , textOnSecondary =
        black
    , link =
        rgb 0.0 0.0 0.9
    , white = white
    , black = black
    , gray =
        rgb 0.4 0.4 0.4
    , lightGray =
        rgb 0.8 0.8 0.8
    }


colorSchemeForUser : Maybe User -> ColorScheme
colorSchemeForUser _ =
    defaultColorScheme
