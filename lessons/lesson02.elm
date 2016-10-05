import Math.Vector3 exposing (..)
import Math.Matrix4 exposing (..)
import WebGL exposing (..)
import Html exposing (Html)
import Html.App as Html
import Html.Attributes exposing (width, height)
import AnimationFrame

-- Create a mesh with a triangle and a square

type alias Vertex = { position : Vec3, color: Vec3 }

triangle : Drawable Vertex
triangle =
  Triangle
    [ ( Vertex (vec3 0 1 0) (vec3 1 0 0)
      , Vertex (vec3 -1 -1 0) (vec3 0 1 0)
      , Vertex (vec3 1 -1 0) (vec3 0 0 1)
      )
    ]

square : Drawable Vertex
square =
  Triangle
    [ ( Vertex (vec3 1 1 0) (vec3 0.5 0.5 1)
      , Vertex (vec3 -1 1 0) (vec3 0.5 0.5 1)
      , Vertex (vec3 1 -1 0) (vec3 0.5 0.5 1)
      ),
      ( Vertex (vec3 -1 1 0) (vec3 0.5 0.5 1)
      , Vertex (vec3 1 -1 0) (vec3 0.5 0.5 1)
      , Vertex (vec3 -1 -1 0) (vec3 0.5 0.5 1)
      )
    ]

main : Html msg
main =
  WebGL.toHtml
    [ width 400, height 400 ]
    ( [render vertexShader fragmentShader triangle { displacement = vec3 -1 0 0}] ++
      [render vertexShader fragmentShader square { displacement = vec3 2 0 0}]
    )

-- Shaders

vertexShader : Shader { attr| position:Vec3, color:Vec3 } { unif | displacement:Vec3 } { vcolor:Vec3 }
vertexShader = [glsl|

  precision mediump float;
  attribute vec3 position;
  attribute vec3 color;
  uniform vec3 displacement;
  varying vec3 vcolor;

  void main() {
    gl_Position = vec4(0.3 * (position + displacement), 1);
    vcolor = color;
  }
|]

fragmentShader : Shader {} u { vcolor:Vec3 }
fragmentShader = [glsl|
  precision mediump float;
  varying vec3 vcolor;

  void main () {
      gl_FragColor = vec4(vcolor, 1.0);
  }

|]
