import Math.Vector3 exposing (..)
import Math.Matrix4 exposing (..)
import WebGL exposing (..)
import Html exposing (Html)
import Html.App as Html
import Html.Attributes exposing (width, height, style)
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
  TriangleStrip
    [ Vertex (vec3 1 1 0) (vec3 0.5 0.5 1)
    , Vertex (vec3 -1 1 0) (vec3 0.5 0.5 1)
    , Vertex (vec3 1 -1 0) (vec3 0.5 0.5 1)
    , Vertex (vec3 -1 -1 0) (vec3 0.5 0.5 1)
    ]

main : Program Never
main =
  Html.program
    { init = (0, Cmd.none)
    , view = view
    , subscriptions = (\model -> AnimationFrame.diffs Basics.identity)
    , update = (\elapsed currentTime -> (elapsed + currentTime, Cmd.none))
    }

-- VIEW

view : Float -> Html msg
view t =
  WebGL.toHtml
    [ width 400, height 400, style [("backgroundColor", "black")] ]
    ( [render vertexShader fragmentShader triangle { displacement = vec3 -1.5 0 0, perspective = perspectiveTriangle (t / 1000)}] ++
      [render vertexShader fragmentShader square { displacement = vec3 1.5 0 0, perspective = perspectiveSquare (t / 1000)}]
    )

perspectiveTriangle : Float -> Mat4
perspectiveTriangle t =
  mul (makePerspective 45 1 0.01 100)
      (makeLookAt (vec3 (4 * cos t) 0 (4 * sin t)) (vec3 0 0 0) (vec3 0 1 0))

perspectiveSquare : Float -> Mat4
perspectiveSquare t =
  mul (makePerspective 45 1 0.01 100)
      (makeLookAt (vec3 0 (4 * cos t) (4 * sin t)) (vec3 0 0 0) (vec3 0 1 0))

-- Shaders

vertexShader : Shader { attr| position:Vec3, color:Vec3 } { unif | displacement:Vec3, perspective:Mat4 } { vcolor:Vec3 }
vertexShader = [glsl|

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

fragmentShader : Shader {} u { vcolor:Vec3 }
fragmentShader = [glsl|
  precision mediump float;
  varying vec3 vcolor;

  void main () {
      gl_FragColor = vec4(vcolor, 1.0);
  }
|]
