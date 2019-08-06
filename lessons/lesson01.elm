module Main exposing (main)

-- Create a mesh with a triangle and a square

import Html exposing (Html)
import Html.Attributes exposing (height, style, width)
import Math.Vector3 exposing (..)
import WebGL exposing (Mesh, Shader)


type alias Vertex =
    { position : Vec3 }


triangle : Mesh Vertex
triangle =
    WebGL.triangles
        [ ( { position = vec3 0 1 0 }
          , { position = vec3 -1 -1 0 }
          , { position = vec3 1 -1 0 }
          )
        ]


square : Mesh Vertex
square =
    WebGL.triangleStrip
        [ { position = vec3 1 1 0 }
        , { position = vec3 -1 1 0 }
        , { position = vec3 1 -1 0 }
        , { position = vec3 -1 -1 0 }
        ]


main : Html msg
main =
    WebGL.toHtml
        [ width 400
        , height 400
        , style "background" "black"
        ]
        [ WebGL.entity vertexShader fragmentShader square { displacement = vec3 -1.5 0 0 }
        , WebGL.entity vertexShader fragmentShader triangle { displacement = vec3 1.5 0 0 }
        ]



-- Shaders


vertexShader : Shader Vertex { unif | displacement : Vec3 } {}
vertexShader =
    [glsl|
  precision mediump float;
  attribute vec3 position;
  uniform vec3 displacement;

  void main() {
    gl_Position = vec4(0.3 * (position + displacement), 1);
  }
|]


fragmentShader : Shader {} u {}
fragmentShader =
    [glsl|
  precision mediump float;

  void main () {
      gl_FragColor = vec4(1, 1, 1, 1);
  }

|]
