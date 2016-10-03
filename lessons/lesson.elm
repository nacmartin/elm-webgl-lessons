import Math.Vector3 exposing (..)
import Math.Matrix4 exposing (..)
import WebGL exposing (..)
import Html exposing (Html)
import Html.App as Html
import Html.Attributes exposing (width, height)
import AnimationFrame

-- Create a mesh with a triangle and a square

type alias Vertex = { position : Vec3 }

mesh : Drawable Vertex
mesh =
  WebGL.Triangle
    [ ( { position = vec3 0 1 0 }
      , { position = vec3 -1 -1 0 }
      , { position = vec3 1 -1 0 }
      )
    ]
-- mesh = Triangle
--   [ ( Vertex (vec3 0  0 0) (vec3 1 0 0)
--     , Vertex (vec3 1  1 0) (vec3 0 1 0)
--     , Vertex (vec3 1 -1 0) (vec3 0 0 1)
--     )
--   ]

main : Html msg
main =
  WebGL.toHtml
    [ width 400, height 400 ]
    [ render vertexShader fragmentShader mesh {} ]

-- Shaders

vertexShader : Shader Vertex {} {}
vertexShader = [glsl|
  precision mediump float;
  attribute vec3 position;
  void main() {
    gl_Position = vec4(0.5 * position, 1);
  }
|]


fragmentShader : Shader {} {} {}
fragmentShader = [glsl|
  precision mediump float;

  void main () {
      gl_FragColor = vec4(0, 0, 0, 1);
  }

|]
