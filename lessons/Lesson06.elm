module Lesson06 exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta, onKeyDown, onKeyUp)
import Html exposing (Html, div, text)
import Html.Attributes exposing (height, style, width)
import Html.Events exposing (keyCode)
import Json.Decode as Decode
import Math.Matrix4 exposing (..)
import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (..)
import Task exposing (Task)
import WebGL exposing (Entity, Mesh, Shader)
import WebGL.Texture as Texture exposing (Error, Texture, defaultOptions)


type alias Model =
    { textures : Maybe ( Texture, Texture )
    , thetaX : Float
    , thetaY : Float
    , textureSelected : Int
    , keys : Keys
    , position : Vec3
    , rx : Float
    , ry : Float
    }


type Msg
    = TexturesLoaded (Result Error ( Texture, Texture ))
    | TextureChange Int
    | KeyChange Bool Int
    | Animate Float


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
    ( { textures = Nothing
      , thetaX = 0
      , thetaY = 0
      , rx = 0
      , ry = 0
      , textureSelected = 1
      , keys = Keys False False False False False False
      , position = vec3 0 0 -4
      }
    , Task.map2 Tuple.pair
        (Texture.loadWith { defaultOptions | minify = Texture.linear, magnify = Texture.linear } "textures/crate.gif")
        (Texture.loadWith { defaultOptions | minify = Texture.nearest, magnify = Texture.nearest } "textures/crate.gif")
        |> Task.attempt TexturesLoaded
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TexturesLoaded textures ->
            ( { model | textures = Result.toMaybe textures }, Cmd.none )

        TextureChange key ->
            ( textureChange key model, Cmd.none )

        KeyChange bool key ->
            ( { model | keys = keyChange bool key model.keys }, Cmd.none )

        Animate dt ->
            ( { model
                | thetaX = model.thetaX + model.rx * dt / 1000
                , thetaY = model.thetaY + model.ry * dt / 1000
                , position = move model.keys model.position
                , rx = rotateX model.keys model.rx
                , ry = rotateY model.keys model.ry
              }
            , Cmd.none
            )


rotateX : { keys | right : Bool, left : Bool } -> Float -> Float
rotateX k velocity =
    let
        direction =
            case ( k.right, k.left ) of
                ( True, False ) ->
                    0.1

                ( False, True ) ->
                    -0.1

                _ ->
                    0
    in
    velocity + direction


rotateY : { keys | up : Bool, down : Bool } -> Float -> Float
rotateY k velocity =
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
    Sub.batch
        [ onAnimationFrameDelta Animate
        , onKeyDown (Decode.map (KeyChange True) keyCode)
        , onKeyUp (Decode.map (KeyChange False) keyCode)
        , onKeyDown (Decode.map TextureChange keyCode)
        ]


textureChange : Int -> Model -> Model
textureChange keyCode m =
    case keyCode of
        70 ->
            if m.textureSelected == 1 then
                { m | textureSelected = 2 }

            else
                { m | textureSelected = 1 }

        _ ->
            m


keyChange : Bool -> Int -> Keys -> Keys
keyChange on keyCode k =
    case keyCode of
        37 ->
            { k | left = on }

        39 ->
            { k | right = on }

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


cube : Mesh { position : Vec3, coord : Vec3 }
cube =
    WebGL.triangles <|
        List.concatMap rotatedFace [ ( 0, 0 ), ( 90, 0 ), ( 180, 0 ), ( 270, 0 ), ( 0, 90 ), ( 0, -90 ) ]


rotatedFace : ( Float, Float ) -> List ( { position : Vec3, coord : Vec3 }, { position : Vec3, coord : Vec3 }, { position : Vec3, coord : Vec3 } )
rotatedFace ( angleX, angleY ) =
    let
        x =
            makeRotate (degrees angleX) (vec3 1 0 0)

        y =
            makeRotate (degrees angleY) (vec3 0 1 0)

        t =
            makeTranslate (vec3 0 0 1)
                |> mul y
                |> mul x

        each f ( a, b, c ) =
            ( f a, f b, f c )
    in
    List.map (each (\x_ -> { x_ | position = transform t x_.position })) face


face : List ( { position : Vec3, coord : Vec3 }, { position : Vec3, coord : Vec3 }, { position : Vec3, coord : Vec3 } )
face =
    let
        topLeft =
            { position = vec3 -1 1 0, coord = vec3 0 1 0 }

        topRight =
            { position = vec3 1 1 0, coord = vec3 1 1 0 }

        bottomLeft =
            { position = vec3 -1 -1 0, coord = vec3 0 0 0 }

        bottomRight =
            { position = vec3 1 -1 0, coord = vec3 1 0 0 }
    in
    [ ( topLeft, topRight, bottomLeft )
    , ( bottomLeft, topRight, bottomRight )
    ]



-- VIEW


view : Model -> Html Msg
view { textures, thetaX, thetaY, textureSelected, position, rx, ry } =
    let
        tex =
            Maybe.map
                (\( texture1, texture2 ) ->
                    if textureSelected == 1 then
                        texture1

                    else
                        texture2
                )
                textures

        entities =
            renderEntity cube thetaX thetaY tex position
    in
    div
        []
        [ WebGL.toHtml
            [ width 400, height 400, style "background" "black" ]
            entities
        , div
            [ style "position" "absolute"
            , style "font-family" "monospace"
            , style "text-align" "center"
            , style "left" "20px"
            , style "right" "20px"
            , style "top" "500px"
            ]
            [ text message ]
        ]


message : String
message =
    "Keys are: F -> change texture mode, Right/Left/Up/Down rotate, w/s -> move camera in/out"


renderEntity : Mesh { position : Vec3, coord : Vec3 } -> Float -> Float -> Maybe Texture -> Vec3 -> List Entity
renderEntity mesh thetaX thetaY texture position =
    case texture of
        Nothing ->
            []

        Just tex ->
            [ WebGL.entity vertexShader fragmentShader mesh (uniformsCube thetaX thetaY tex position) ]


uniformsCube : Float -> Float -> Texture -> Vec3 -> { texture : Texture, rotation : Mat4, perspective : Mat4, camera : Mat4, displacement : Vec3 }
uniformsCube tx ty texture displacement =
    { texture = texture
    , rotation =
        makeRotate 0 (vec3 0 0 1)
            |> mul (makeRotate tx (vec3 0 1 0))
            |> mul (makeRotate ty (vec3 1 0 0))
    , perspective = makePerspective 45 1 0.01 100
    , camera = makeLookAt displacement (add displacement k) (vec3 0 1 0)
    , displacement = vec3 0 0 0
    }



-- SHADERS


vertexShader : Shader { attr | position : Vec3, coord : Vec3 } { unif | rotation : Mat4, displacement : Vec3, perspective : Mat4, camera : Mat4 } { vcoord : Vec2 }
vertexShader =
    [glsl|

  precision mediump float;
  attribute vec3 position;
  attribute vec3 coord;
  uniform mat4 rotation;
  uniform vec3 displacement;
  uniform mat4 perspective;
  uniform mat4 camera;
  varying vec2 vcoord;

  void main() {
    gl_Position = perspective * camera * rotation * vec4(position, 1.0) + vec4(displacement, 1);
    vcoord = coord.xy;
  }
|]


fragmentShader : Shader {} { unif | texture : Texture } { vcoord : Vec2 }
fragmentShader =
    [glsl|
  precision mediump float;
  uniform sampler2D texture;
  varying vec2 vcoord;

  void main () {
      gl_FragColor = texture2D(texture, vcoord);
  }

|]
