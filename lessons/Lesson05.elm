module Lesson05 exposing (main)

-- Rotating cube mesh with texture

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Html exposing (Html)
import Html.Attributes exposing (height, style, width)
import Math.Matrix4 exposing (..)
import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (..)
import Task
import WebGL exposing (Mesh, Shader)
import WebGL.Texture as Texture exposing (Error, Texture)


type alias Model =
    { texture : Maybe Texture
    , theta : Float
    }


type Msg
    = TextureLoaded (Result Error Texture)
    | Animate Float


init : ( Model, Cmd Msg )
init =
    ( { texture = Nothing, theta = 0 }
    , Task.attempt TextureLoaded (Texture.load "textures/nehe.gif")
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TextureLoaded texture ->
            ( { model | texture = Result.toMaybe texture }, Cmd.none )

        Animate dt ->
            ( { model | theta = model.theta + dt / 1000 }, Cmd.none )


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> init
        , view = view
        , subscriptions = \model -> onAnimationFrameDelta Animate
        , update = update
        }



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
view { texture, theta } =
    (case texture of
        Nothing ->
            []

        Just tex ->
            [ WebGL.entity vertexShader fragmentShader cube (uniformsCube theta tex) ]
    )
        |> WebGL.toHtml [ width 400, height 400, style "background" "black" ]


uniformsCube : Float -> Texture -> { texture : Texture, rotation : Mat4, perspective : Mat4, camera : Mat4, displacement : Vec3 }
uniformsCube t texture =
    { texture = texture
    , rotation =
        makeRotate t (vec3 0 0 1)
            |> mul (makeRotate t (vec3 0 1 0))
            |> mul (makeRotate t (vec3 1 0 0))
    , perspective = makePerspective 45 1 0.01 100
    , camera = makeLookAt (vec3 0 0 5) (vec3 0 0 0) (vec3 0 1 0)
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
