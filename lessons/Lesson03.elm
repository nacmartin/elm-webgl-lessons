module Lesson03 exposing (main)

-- Create a mesh with a triangle and a square
-- Uses varyings to interpolate colors
-- Uses uniforms to animate the scene

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Html exposing (Html)
import Html.Attributes exposing (height, style, width)
import Math.Matrix4 exposing (..)
import Math.Vector3 exposing (..)
import WebGL exposing (Mesh, Shader)


type alias Vertex =
    { position : Vec3, color : Vec3 }


triangle : Mesh Vertex
triangle =
    WebGL.triangles
        [ ( Vertex (vec3 0 1 0) (vec3 1 0 0)
          , Vertex (vec3 -1 -1 0) (vec3 0 1 0)
          , Vertex (vec3 1 -1 0) (vec3 0 0 1)
          )
        ]


square : Mesh Vertex
square =
    WebGL.triangleStrip
        [ Vertex (vec3 1 1 0) (vec3 0.5 0.5 1)
        , Vertex (vec3 -1 1 0) (vec3 0.5 0.5 1)
        , Vertex (vec3 1 -1 0) (vec3 0.5 0.5 1)
        , Vertex (vec3 -1 -1 0) (vec3 0.5 0.5 1)
        ]


main : Program () Float Float
main =
    Browser.element
        { init = \_ -> ( 0, Cmd.none )
        , view = view
        , subscriptions = \_ -> onAnimationFrameDelta Basics.identity
        , update = \elapsed currentTime -> ( elapsed + currentTime, Cmd.none )
        }



-- VIEW


view : Float -> Html msg
view t =
    WebGL.toHtml
        [ width 400
        , height 400
        , style "background" "black"
        ]
        [ WebGL.entity vertexShader fragmentShader triangle { displacement = vec3 -1.5 0 0, perspective = perspectiveTriangle (t / 1000) }
        , WebGL.entity vertexShader fragmentShader square { displacement = vec3 1.5 0 0, perspective = perspectiveSquare (t / 1000) }
        ]


perspectiveTriangle : Float -> Mat4
perspectiveTriangle t =
    mul (makePerspective 45 1 0.01 100)
        (makeLookAt (vec3 (4 * cos t) 0 (4 * sin t)) (vec3 0 0 0) (vec3 0 1 0))


perspectiveSquare : Float -> Mat4
perspectiveSquare t =
    mul (makePerspective 45 1 0.01 100)
        (makeLookAt (vec3 0 (4 * cos t) (4 * sin t)) (vec3 0 0 0) (vec3 0 1 0))



-- Shaders


vertexShader : Shader { attr | position : Vec3, color : Vec3 } { unif | displacement : Vec3, perspective : Mat4 } { vcolor : Vec3 }
vertexShader =
    [glsl|

  precision mediump float;
  attribute vec3 position;
  attribute vec3 color;
  uniform vec3 displacement;
  uniform mat4 perspective;
  varying vec3 vcolor;

  void main() {
    gl_Position = perspective * vec4(0.5 * position, 1) + vec4(displacement, 1);
    vcolor = color;
  }
|]


fragmentShader : Shader {} u { vcolor : Vec3 }
fragmentShader =
    [glsl|
  precision mediump float;
  varying vec3 vcolor;

  void main () {
      gl_FragColor = vec4(vcolor, 1.0);
  }
|]
