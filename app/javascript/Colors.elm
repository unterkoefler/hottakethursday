module Colors exposing (ColorScheme, colorSchemeForUser)

import Css.Colors as CssColors
import Data.User exposing (User)
import Dict exposing (Dict)
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


cssColorDict : Dict String Element.Color
cssColorDict =
    Dict.fromList
        [ ( "aliceblue", CssColors.aliceblue )
        , ( "antiquewhite", CssColors.antiquewhite )
        , ( "aqua", CssColors.aqua )
        , ( "aquamarine", CssColors.aquamarine )
        , ( "azure", CssColors.azure )
        , ( "beige", CssColors.beige )
        , ( "bisque", CssColors.bisque )
        , ( "black", CssColors.black )
        , ( "blanchedalmond", CssColors.blanchedalmond )
        , ( "blue", CssColors.blue )
        , ( "blueviolet", CssColors.blueviolet )
        , ( "brown", CssColors.brown )
        , ( "burlywood", CssColors.burlywood )
        , ( "cadetblue", CssColors.cadetblue )
        , ( "chartreuse", CssColors.chartreuse )
        , ( "chocolate", CssColors.chocolate )
        , ( "coral", CssColors.coral )
        , ( "cornflowerblue", CssColors.cornflowerblue )
        , ( "cornsilk", CssColors.cornsilk )
        , ( "crimson", CssColors.crimson )
        , ( "cyan", CssColors.cyan )
        , ( "darkblue", CssColors.darkblue )
        , ( "darkcyan", CssColors.darkcyan )
        , ( "darkgoldenrod", CssColors.darkgoldenrod )
        , ( "darkgray", CssColors.darkgray )
        , ( "darkgreen", CssColors.darkgreen )
        , ( "darkgrey", CssColors.darkgrey )
        , ( "darkkhaki", CssColors.darkkhaki )
        , ( "darkmagenta", CssColors.darkmagenta )
        , ( "darkolivegreen", CssColors.darkolivegreen )
        , ( "darkorange", CssColors.darkorange )
        , ( "darkorchid", CssColors.darkorchid )
        , ( "darkred", CssColors.darkred )
        , ( "darksalmon", CssColors.darksalmon )
        , ( "darkseagreen", CssColors.darkseagreen )
        , ( "darkslateblue", CssColors.darkslateblue )
        , ( "darkslategray", CssColors.darkslategray )
        , ( "darkturquoise", CssColors.darkturquoise )
        , ( "darkviolet", CssColors.darkviolet )
        , ( "deeppink", CssColors.deeppink )
        , ( "deepskyblue", CssColors.deepskyblue )
        , ( "dimgray", CssColors.dimgray )
        , ( "dodgerblue", CssColors.dodgerblue )
        , ( "firebrick", CssColors.firebrick )
        , ( "floralwhite", CssColors.floralwhite )
        , ( "forestgreen", CssColors.forestgreen )
        , ( "fuchsia", CssColors.fuchsia )
        , ( "gainsboro", CssColors.gainsboro )
        , ( "ghostwhite", CssColors.ghostwhite )
        , ( "gold", CssColors.gold )
        , ( "goldenrod", CssColors.goldenrod )
        , ( "gray", CssColors.gray )
        , ( "green", CssColors.green )
        , ( "greenyellow", CssColors.greenyellow )
        , ( "grey", CssColors.grey )
        , ( "honeydew", CssColors.honeydew )
        , ( "hotpink", CssColors.hotpink )
        , ( "indianred", CssColors.indianred )
        , ( "indigo", CssColors.indigo )
        , ( "ivory", CssColors.ivory )
        , ( "khaki", CssColors.khaki )
        , ( "lavender", CssColors.lavender )
        , ( "lavenderblush", CssColors.lavenderblush )
        , ( "lawngreen", CssColors.lawngreen )
        , ( "lemonchiffon", CssColors.lemonchiffon )
        , ( "lightblue", CssColors.lightblue )
        , ( "lightcoral", CssColors.lightcoral )
        , ( "lightcyan", CssColors.lightcyan )
        , ( "lightgoldenrodyellow", CssColors.lightgoldenrodyellow )
        , ( "lightgray", CssColors.lightgray )
        , ( "lightgreen", CssColors.lightgreen )
        , ( "lightgrey", CssColors.lightgrey )
        , ( "lightpink", CssColors.lightpink )
        , ( "lightsalmon", CssColors.lightsalmon )
        , ( "lightseagreen", CssColors.lightseagreen )
        , ( "lightskyblue", CssColors.lightskyblue )
        , ( "lightslategray", CssColors.lightslategray )
        , ( "lightslategrey", CssColors.lightslategrey )
        , ( "lightsteelblue", CssColors.lightsteelblue )
        , ( "lightyellow", CssColors.lightyellow )
        , ( "lime", CssColors.lime )
        , ( "limegreen", CssColors.limegreen )
        , ( "linen", CssColors.linen )
        , ( "magenta", CssColors.magenta )
        , ( "maroon", CssColors.maroon )
        , ( "mediumaquamarine", CssColors.mediumaquamarine )
        , ( "mediumblue", CssColors.mediumblue )
        , ( "mediumorchid", CssColors.mediumorchid )
        , ( "mediumpurple", CssColors.mediumpurple )
        , ( "mediumseagreen", CssColors.mediumseagreen )
        , ( "mediumslateblue", CssColors.mediumslateblue )
        , ( "mediumspringgreen", CssColors.mediumspringgreen )
        , ( "mediumturquoise", CssColors.mediumturquoise )
        , ( "mediumvioletred", CssColors.mediumvioletred )
        , ( "midnightblue", CssColors.midnightblue )
        , ( "mintcream", CssColors.mintcream )
        , ( "mistyrose", CssColors.mistyrose )
        , ( "moccasin", CssColors.moccasin )
        , ( "navajowhite", CssColors.navajowhite )
        , ( "navy", CssColors.navy )
        , ( "oldlace", CssColors.oldlace )
        , ( "olive", CssColors.olive )
        , ( "olivedrab", CssColors.olivedrab )
        , ( "orange", CssColors.orange )
        , ( "orangered", CssColors.orangered )
        , ( "orchid", CssColors.orchid )
        , ( "palegoldenrod", CssColors.palegoldenrod )
        , ( "palegreen", CssColors.palegreen )
        , ( "paleturquoise", CssColors.paleturquoise )
        , ( "palevioletred", CssColors.palevioletred )
        , ( "papayawhip", CssColors.papayawhip )
        , ( "peachpuff", CssColors.peachpuff )
        , ( "peru", CssColors.peru )
        , ( "pink", CssColors.pink )
        , ( "plum", CssColors.plum )
        , ( "powderblue", CssColors.powderblue )
        , ( "purple", CssColors.purple )
        , ( "rebeccapurple", CssColors.rebeccapurple )
        , ( "red", CssColors.red )
        , ( "rosybrown", CssColors.rosybrown )
        , ( "royalblue", CssColors.royalblue )
        , ( "saddlebrown", CssColors.saddlebrown )
        , ( "salmon", CssColors.salmon )
        , ( "sandybrown", CssColors.sandybrown )
        , ( "seagreen", CssColors.seagreen )
        , ( "seashell", CssColors.seashell )
        , ( "sienna", CssColors.sienna )
        , ( "silver", CssColors.silver )
        , ( "skyblue", CssColors.skyblue )
        , ( "slateblue", CssColors.slateblue )
        , ( "slategray", CssColors.slategray )
        , ( "slategrey", CssColors.slategrey )
        , ( "snow", CssColors.snow )
        , ( "springgreen", CssColors.springgreen )
        , ( "steelblue", CssColors.steelblue )
        , ( "tan", CssColors.tan )
        , ( "teal", CssColors.teal )
        , ( "thistle", CssColors.thistle )
        , ( "tomato", CssColors.tomato )
        , ( "turquoise", CssColors.turquoise )
        , ( "violet", CssColors.violet )
        , ( "wheat", CssColors.wheat )
        , ( "white", CssColors.white )
        , ( "whitesmoke", CssColors.whitesmoke )
        , ( "yellow", CssColors.yellow )
        , ( "yellowgreen", CssColors.yellowgreen )
        ]
        |> Dict.map (\_ c -> Element.rgba255 c.red c.green c.blue c.alpha)


luminance : Color -> Float
luminance color =
    let
        { red, green, blue } = Element.toRgb color
        r = if red <= 0.03928 then red / 12.92 else ((red + 0.055) / 1.055) ^ 2.4
        g = if green <= 0.03928 then green / 12.92 else ((green + 0.055) / 1.055) ^ 2.4
        b = if blue <= 0.03928 then blue / 12.92 else ((blue + 0.055) / 1.055) ^ 2.4
    in
        0.2126 * r + 0.7152 * g + 0.0722 * b


colorSchemeForUser : Maybe User -> ColorScheme
colorSchemeForUser maybeUser =
    case maybeUser of
        Nothing ->
            defaultColorScheme

        Just { leastFavoriteColor } ->
            let
                cleaned =
                    leastFavoriteColor
                        |> String.toLower
                        |> String.filter Char.isAlpha
            in
            case Dict.get cleaned cssColorDict of
                Just color ->
                    { defaultColorScheme | primary = color, textOnPrimary = if 1.05 / ((luminance color) + 0.05) < 4.5 then CssColors.black else CssColors.white }

                Nothing ->
                    defaultColorScheme
