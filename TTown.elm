module TTown exposing (..)

import Html exposing (img, div)
import Html.Attributes exposing (src, width, id)
import Css
import Time exposing (Time)
import AnimationFrame


-- http://yourjavascript.com/01187126411/ttown.js
--MODEL


type alias Model =
    { time : Float
    , presidents : List President
    }


init : Model
init =
    { time = 0
    , presidents =
        [ { emptyPresident | species = Just Moon }
        , { emptyPresident | species = Just (Circle defaultCircleData) }
        ]
            ++ (presidentialWave True)
            ++ (presidentialWave False)
    }


defaultCircleData : CircleData
defaultCircleData =
    { delta = 0, speed = 0.5, radius = 0.5, center = ( 0, 0 ), size = Small }


presidentialWave : Bool -> List President
presidentialWave flag =
    let
        y0 =
            0.2

        minR =
            0.1

        n =
            20

        ( x0, maxR, speed ) =
            if flag then
                ( 1.2, 1.2, -0.5 )
            else
                ( -0.5, 1.2, 0.5 )

        sizes =
            [ Small, Small, Small, Medium, Medium, Large, Yuge ]

        sizeMod r =
            (r * 77) % (List.length sizes) |> (flip List.drop) sizes |> List.head |> Maybe.withDefault Small

        radii =
            linspace minR maxR n |> Debug.log "radii"

        circleData =
            { delta = pi, speed = speed, radius = 0, center = ( x0, y0 ), size = Medium }

        pres =
            { x = 0, y = 0, angle = 0, size = 0, species = Nothing }
    in
        radii
            |> List.map (\r -> { circleData | radius = r, delta = r * 17, size = sizeMod r })
            |> List.map (\cd -> { pres | species = Just (Circle cd) })


type Species
    = Moon
    | Circle CircleData


type Size
    = Small
    | Medium
    | Large
    | Yuge


type alias CircleData =
    { delta : Float
    , speed : Float
    , radius : Float
    , center : ( Float, Float )
    , size : Size
    }


type alias President =
    { x : Float
    , y : Float
    , angle : Float
    , size : Float
    , species : Maybe Species
    }


emptyPresident : President
emptyPresident =
    { x = 0, y = 0, angle = 0, size = 0, species = Nothing }


main : Program Never Model Action
main =
    Html.program
        { init = init ! []
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- UPDATE


type Action
    = Animate Time.Time


update : Action -> Model -> ( Model, Cmd Action )
update action model =
    case action of
        Animate dt ->
            let
                time =
                    model.time + dt / 1000

                presidents =
                    model.presidents |> List.map (updatePresident time)
            in
                { model | time = time, presidents = presidents } ! []


updatePresident : Time -> President -> President
updatePresident time president =
    case president.species of
        Just Moon ->
            redMoonRising time

        Just (Circle circleData) ->
            vomit time circleData

        Nothing ->
            emptyPresident


redMoonRising : Time -> President
redMoonRising time =
    let
        maxTime =
            1

        start =
            -0.5

        end =
            0.2

        a =
            clamp 0 1 (time / maxTime)

        y =
            start * (1 - a) + end * a
    in
        { x = 0.05, y = y, angle = 0.1, size = 0.8, species = Just Moon }


vomit : Time -> CircleData -> President
vomit time ({ speed, delta, radius, size, center } as circleData) =
    let
        ( x0, y0 ) =
            center

        x =
            (cos (time * speed + delta)) * radius + x0 - trueSize / 2

        y =
            (sin (time * speed + delta)) * radius + y0 - trueSize / 2

        heading =
            if speed > 0 then
                0
            else
                pi

        angle =
            (time * speed + delta) + heading

        trueSize =
            case size of
                Small ->
                    0.05

                Medium ->
                    0.15

                Large ->
                    0.25

                Yuge ->
                    0.5
    in
        { x = x, y = y, angle = angle, size = trueSize, species = Just (Circle circleData) }



-- VIEW


boxStyle : Html.Attribute msg
boxStyle =
    [ Css.overflow Css.hidden, Css.border (Css.px 10), Css.position Css.relative, Css.width (Css.px 771), Css.height (Css.px 459) ] |> styles


inlayStyle : Int -> Html.Attribute msg
inlayStyle z =
    [ Css.position Css.absolute, Css.property "z-index" (z |> toString) ] |> styles


presidentStyle : Int -> Html.Attribute msg
presidentStyle z =
    [ Css.position Css.absolute, Css.property "z-index" (z |> toString) ] |> styles


view : Model -> Html.Html msg
view model =
    let
        land =
            img [ src landSrc, id "land", inlayStyle 3 ] []

        sky =
            img [ src skySrc, id "sky", inlayStyle 1 ] []

        presidents =
            model.presidents |> List.map inflatePresident
    in
        div [ id "house", boxStyle ] ([ sky, land ] ++ presidents)


inflatePresident : President -> Html.Html msg
inflatePresident { x, y, size, angle } =
    let
        -- CSS rotate is clock-wise...
        deg =
            angle * (-180 / pi) |> Css.deg

        pct x =
            Css.pct (x * 100)

        style =
            [ Css.left (pct x)
            , Css.bottom (pct y)
            , Css.width (pct size)
            , Css.height (pct size)
            ]
                |> (::) (Css.transforms [ Css.rotate deg ])
                |> styles
    in
        img [ src headSrc, presidentStyle 2, style ] []


landSrc : String
landSrc =
    "http://i.imgur.com/Lzsspw1.png"


skySrc : String
skySrc =
    "http://i.imgur.com/7Seymp8.png"


headSrc : String
headSrc =
    "http://i.imgur.com/HlKRt0C.png"


styles : List Css.Mixin -> Html.Attribute msg
styles =
    Css.asPairs >> Html.Attributes.style


linspace : number -> number -> Int -> List number
linspace start end n =
    let
        n_ =
            toFloat n

        xs =
            List.range 0 (n - 1) |> List.map (\x -> (toFloat x) / (n_ - 1))
    in
        xs
            |> List.map (\x -> start * (1 - x) + end * x)


subscriptions : Model -> Sub Action
subscriptions model =
    [ AnimationFrame.diffs Animate ] |> Sub.batch
