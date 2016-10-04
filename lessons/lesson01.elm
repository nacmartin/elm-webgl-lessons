import Math.Vector3 exposing (..)
import Math.Matrix4 exposing (..)
import WebGL exposing (..)
import Html exposing (Html)
import Html.App as Html
import Html.Attributes exposing (width, height)
import AnimationFrame

-- Create a mesh with a triangle and a square

type alias Vertex = { position : Vec3 }

triangle : Drawable Vertex
triangle =
  WebGL.Triangle
    [ (
        { position = vec3 0 1 0 },
        { position = vec3 -1 -1 0 },
        { position = vec3 1 -1 0 }
      )
    ]

square : Drawable Vertex
square =
  WebGL.Triangle
    [ (
        { position = vec3 1 1 0 },
        { position = vec3 -1 1 0 },
        { position = vec3 1 -1 0 }
      ),
      (
        { position = vec3 -1 1 0 },
        { position = vec3 1 -1 0 },
        { position = vec3 -1 -1 0 }
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

vertexShader : Shader Vertex { unif | displacement:Vec3 } {}
vertexShader = [glsl|
  precision mediump float;
  attribute vec3 position;
  uniform vec3 displacement;

  void main() {
    gl_Position = vec4(0.3 * (position + displacement), 1);
  }
|]

fragmentShader : Shader {} u {}
fragmentShader = [glsl|
  precision mediump float;

  void main () {
      gl_FragColor = vec4(0, 0, 0, 1);
  }

|]
