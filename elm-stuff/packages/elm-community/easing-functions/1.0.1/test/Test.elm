module Test exposing (..)

import Ease exposing (..)
import Collage exposing (..)
import Element exposing (toHtml)
import Text
import Color exposing (gray, black)


width =
  120


height =
  64


plot : Int -> ( Easing, String ) -> Form
plot i ( f, name ) =
  group
    [ rect width 1 |> filled gray
    , rect width 1 |> filled gray |> moveY height
    , path
        (List.map (\x -> ( x - width / 2, f (x / width) * height )) [0..width])
        |> traced (solid black)
      --|> (\ls -> {ls | width = 3}),
    , Text.fromString name |> text |> moveY (height + 8)
    ]
    |> move ( (width + 10) * (i % 6) |> toFloat, (-height - 20) * (i // 6) |> toFloat )


title =
  Text.fromString "This is a replication of easings.net for testing purposes. You can see the plots are nearly identical."
    |> text
    |> move ( 200, height + 30 )


main =
  let
    forms =
      title :: List.indexedMap plot easingFunctions

    grouped =
      group forms |> move ( -200, 300 )
  in
    collage 1000 1000 [ grouped ] |> toHtml


easingFunctions =
  [ ( inSine, "inSine" )
  , ( outSine, "outSine" )
  , ( inOutSine, "inOutSine" )
  , ( inQuad, "inQuad" )
  , ( outQuad, "outQuad" )
  , ( inOutQuad, "inOutQuad" )
  , ( inCubic, "inCubic" )
  , ( outCubic, "outCubic" )
  , ( inOutCubic, "inOutCubic" )
  , ( inQuart, "inQuart" )
  , ( outQuart, "outQuart" )
  , ( inOutQuart, "inOutQuart" )
  , ( inQuint, "inQuint" )
  , ( outQuint, "outQuint" )
  , ( inOutQuint, "inOutQuint" )
  , ( inExpo, "inExpo" )
  , ( outExpo, "outExpo" )
  , ( inOutExpo, "inOutExpo" )
  , ( inCirc, "inCirc" )
  , ( outCirc, "outCirc" )
  , ( inOutCirc, "inOutCirc" )
  , ( inBack, "inBack" )
  , ( outBack, "outBack" )
  , ( inOutBack, "inOutBack" )
  , ( inElastic, "inElastic" )
  , ( outElastic, "outElastic" )
  , ( inOutElastic, "inOutElastic" )
  , ( inBounce, "inBounce" )
  , ( outBounce, "outBounce" )
  , ( inOutBounce, "inOutBounce" )
  ]
