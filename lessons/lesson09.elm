module Main exposing (main)

import Array
import Browser
import Browser.Events exposing (onAnimationFrameDelta, onKeyDown, onKeyUp)
import Html exposing (Html, div, text)
import Html.Attributes exposing (height, style, width)
import Html.Events exposing (keyCode)
import Json.Decode as Decode
import Math.Matrix4 exposing (..)
import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (..)
import Random
import Task exposing (Task)
import WebGL exposing (Entity, Mesh, Shader)
import WebGL.Settings.Blend as Blend
import WebGL.Texture as Texture exposing (Error, Texture, defaultOptions)


effectiveFPMS : Float
effectiveFPMS =
    30.0 / 10000.0


type alias Model =
    { texture : Maybe Texture
    , stars : List Star
    , keys : Keys
    , position : Vec3
    , tilt : Float
    , spin : Float
    }


type alias Star =
    { dist : Float
    , color : Vec3
    , rotationSpeed : Float
    , angle : Float
    }


type Msg
    = TextureLoaded (Result Error Texture)
    | KeyChange Bool Int
    | Animate Float
    | RandomColorGenerated RandomColor


type alias RandomColor =
    { idx : Int, component : Int, value : Float }


type alias Keys =
    { left : Bool
    , right : Bool
    , up : Bool
    , down : Bool
    , w : Bool
    , s : Bool
    }


init : ( Model, Cmd Msg )
init =
    ( { texture = Nothing
      , tilt = degrees 90
      , spin = 0
      , position = vec3 0 0 20
      , stars = initStars 50
      , keys = Keys False False False False False False
      }
    , Cmd.batch
        [ Texture.loadWith { defaultOptions | minify = Texture.linear, magnify = Texture.linear } "textures/star.gif"
            |> Task.attempt TextureLoaded
        , randomColors 50
        ]
    )


initStars : Int -> List Star
initStars num =
    List.map (initStar num) (List.range 1 num)


initStar : Int -> Int -> Star
initStar total index =
    { angle = 0.0
    , dist = 5.0 * toFloat index / toFloat total
    , rotationSpeed = toFloat index / toFloat total
    , color = vec3 1.0 1.0 1.0
    }


generator =
    Random.float 0 1


randomColors : Int -> Cmd Msg
randomColors total =
    Cmd.batch (List.concat (List.map (\c -> List.map (\idx -> randomColor idx c) (List.range 0 total)) (List.range 0 2)))


randomColor : Int -> Int -> Cmd Msg
randomColor starIdx component =
    Random.generate (\res -> RandomColorGenerated { idx = starIdx, component = component, value = res }) generator


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TextureLoaded texture ->
            ( { model | texture = Result.toMaybe texture }, Cmd.none )

        KeyChange on key ->
            ( { model | keys = keyChange on key model.keys }, Cmd.none )

        Animate dt ->
            ( { model
                | position =
                    model.position
                        |> move model.keys
                , tilt =
                    model.tilt
                        |> rotateX model.keys
                , spin = model.spin + 0.1
                , stars = List.map (updateStar dt) model.stars
              }
            , Cmd.none
            )

        RandomColorGenerated color ->
            ( { model | stars = replaceColorStars color model.stars }, Cmd.none )


replaceColorStars : RandomColor -> List Star -> List Star
replaceColorStars { idx, component, value } stars =
    let
        newStar =
            get idx stars
    in
    case newStar of
        Nothing ->
            stars

        Just someStar ->
            updateStarInList stars idx (replaceColor component value someStar)


get n xs =
    List.head (List.drop n xs)


replaceColor : Int -> Float -> Star -> Star
replaceColor component value star =
    case component of
        0 ->
            { star | color = setX value star.color }

        1 ->
            { star | color = setY value star.color }

        2 ->
            { star | color = setZ value star.color }

        _ ->
            star


updateStarInList : List Star -> Int -> Star -> List Star
updateStarInList list indexToUpdate star =
    let
        updateEl index prevStar =
            if index == indexToUpdate then
                star

            else
                prevStar
    in
    List.indexedMap updateEl list


updateStar : Float -> Star -> Star
updateStar dt star =
    { star
        | angle = star.angle + star.rotationSpeed * dt * effectiveFPMS
        , dist =
            if star.dist < 0.0 then
                star.dist + 5.0

            else
                star.dist - 0.1 * dt * effectiveFPMS
    }


rotateX : { keys | up : Bool, down : Bool } -> Float -> Float
rotateX k velocity =
    let
        direction =
            case ( k.up, k.down ) of
                ( True, False ) ->
                    0.1

                ( False, True ) ->
                    -0.1

                _ ->
                    0
    in
    velocity + direction


move : { keys | w : Bool, s : Bool } -> Vec3 -> Vec3
move k position =
    let
        direction =
            case ( k.w, k.s ) of
                ( True, False ) ->
                    0.1

                ( False, True ) ->
                    -0.1

                _ ->
                    0
    in
    add position (vec3 0 0 direction)


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> init
        , view = view
        , subscriptions = subscriptions
        , update = update
        }


subscriptions : Model -> Sub Msg
subscriptions _ =
    [ onAnimationFrameDelta Animate
    , onKeyDown (Decode.map (KeyChange True) keyCode)
    , onKeyUp (Decode.map (KeyChange False) keyCode)
    ]
        |> Sub.batch


keyChange : Bool -> Int -> Keys -> Keys
keyChange on keyCode k =
    case keyCode of
        38 ->
            { k | up = on }

        40 ->
            { k | down = on }

        87 ->
            { k | w = on }

        83 ->
            { k | s = on }

        _ ->
            k



-- MESHES


starMesh : Mesh { position : Vec3, coord : Vec3 }
starMesh =
    WebGL.triangles
        [ ( { position = vec3 -1 1 0, coord = vec3 0 1 0 }
          , { position = vec3 1 1 0, coord = vec3 1 1 0 }
          , { position = vec3 -1 -1 0, coord = vec3 0 0 0 }
          )
        , ( { position = vec3 -1 -1 0, coord = vec3 0 0 0 }
          , { position = vec3 1 1 0, coord = vec3 1 1 0 }
          , { position = vec3 1 -1 0, coord = vec3 1 0 0 }
          )
        ]



-- VIEW


view : Model -> Html Msg
view { texture, position, tilt, stars, spin } =
    let
        entities =
            List.concat (List.map (renderStar tilt texture position spin) stars)
    in
    div
        []
        [ WebGL.toHtml
            [ width 500, height 500, style "background" "black" ]
            entities
        , div
            [ style "font-family" "monospace"
            , style "left" "20px"
            , style "right" "20px"
            , style "top" "500px"
            ]
            [ text message ]
        ]


message : String
message =
    "Up/Down rotate, w/s -> move camera in/out"


renderStar : Float -> Maybe Texture -> Vec3 -> Float -> Star -> List Entity
renderStar tilt texture position spin star =
    renderEntity starMesh tilt texture position spin star


renderEntity : Mesh { position : Vec3, coord : Vec3 } -> Float -> Maybe Texture -> Vec3 -> Float -> Star -> List Entity
renderEntity mesh tilt texture position spin star =
    case texture of
        Nothing ->
            []

        Just tex ->
            [ WebGL.entityWith [ Blend.add Blend.srcAlpha Blend.one ] vertexShader fragmentShader mesh (uniformsStar tilt tex position spin star) ]


uniformsStar : Float -> Texture -> Vec3 -> Float -> Star -> { texture : Texture, perspective : Mat4, camera : Mat4, worldSpace : Mat4, color : Vec3 }
uniformsStar tilt texture position spin { dist, rotationSpeed, color, angle } =
    { texture = texture
    , worldSpace =
        makeRotate spin (vec3 0 0 1)
            |> mul (makeRotate tilt (vec3 -1 0 0))
            |> mul (makeRotate angle (vec3 0 -1 0))
            |> mul (makeTranslate (vec3 dist 0 0))
            |> mul (makeRotate angle (vec3 0 1 0))
            |> mul (makeRotate tilt (vec3 1 0 0))
    , camera = makeLookAt position (vec3 0 0 -1) (vec3 0 1 0)
    , perspective = makePerspective 45 1 0.01 100
    , color = color
    }



-- SHADERS


vertexShader : Shader { attr | position : Vec3, coord : Vec3 } { unif | worldSpace : Mat4, perspective : Mat4, camera : Mat4 } { vcoord : Vec2 }
vertexShader =
    [glsl|

  precision mediump float;
  attribute vec3 position;
  attribute vec3 coord;
  uniform mat4 worldSpace;
  uniform mat4 perspective;
  uniform mat4 camera;
  varying vec2 vcoord;

  void main() {
    gl_Position = perspective * camera * worldSpace * vec4(position, 1.0);
    vcoord = coord.xy;
  }
|]


fragmentShader : Shader {} { unif | texture : Texture, color : Vec3 } { vcoord : Vec2 }
fragmentShader =
    [glsl|
  precision mediump float;
  uniform sampler2D texture;
  uniform vec3 color;
  varying vec2 vcoord;

  void main () {
      gl_FragColor = texture2D(texture, vcoord) * vec4(color, 1.0);
  }

|]
