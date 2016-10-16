import Debug
import Keyboard
import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (..)
import Math.Matrix4 exposing (..)
import Task exposing (Task)
import Time exposing (Time)
import WebGL exposing (..)
import Html exposing (Html, text, div)
import Html.App as Html
import AnimationFrame
import Html.Attributes exposing (width, height, style)

type alias Model =
  { texture : Maybe Texture
  , keys : Keys
  , stars: List Star
  , rx: Float
  , ry: Float
  , position: Vec3
  }

type alias Star =
  { dist: Float
  , color: Vec3
  , rotationSpeed: Float
  , angle: Float
  }

type Action
  = TexturesError Error
  | TexturesLoaded (Maybe Texture)
  | KeyChange (Keys -> Keys)
  | Animate Time

type alias Keys =
  { left : Bool
  , right : Bool
  , up : Bool
  , down : Bool
  , w : Bool
  , s : Bool
  }

init : (Model, Cmd Action)
init =
  ( {texture = Nothing
  , rx = 0
  , ry = 0
  , position = vec3 0 0 4
  , stars = (initStars 50)
  , keys = Keys False False False False False False
  }
  , fetchTexture |> Task.perform TexturesError TexturesLoaded
  )

initStars : Int -> List Star
initStars num =
  List.map (initStar num) (List.repeat num 1)

initStar : Int -> Int -> Star
initStar total index =
  { angle = 0.0
  , dist = 5.0 * toFloat(index) / toFloat(total)
  , rotationSpeed = toFloat(index) / toFloat(total)
  , color = vec3 1 0.5 0.5
  }

fetchTexture : Task Error (Maybe Texture)
fetchTexture =
  loadTextureWithFilter WebGL.Linear "textures/star.gif" `Task.andThen` \linearTexture ->
  Task.succeed (Just linearTexture)

update : Action -> Model -> (Model, Cmd Action)
update action model =
  case action of
    TexturesError err ->
      (model, Cmd.none)
    TexturesLoaded texture ->
      ({model | texture = texture}, Cmd.none)
    KeyChange keyfunc ->
      ({model | keys = keyfunc model.keys}, Cmd.none)
    Animate dt ->
      ( { model
        | position = model.position
            |> move model.keys
        , rx = model.rx
            |> rotateX model.keys
        , ry = model.ry
            |> rotateY model.keys
        }
        , Cmd.none
      )

rotateX : {keys| right: Bool, left: Bool} -> Float -> Float
rotateX k velocity =
  let
    direction =
      case (k.right, k.left) of
        (True, False) -> 0.1
        (False, True) -> -0.1
        _ -> 0
  in
     velocity + direction

rotateY : {keys| up: Bool, down: Bool} -> Float -> Float
rotateY k velocity =
  let
    direction =
      case (k.up, k.down) of
        (True, False) -> 0.1
        (False, True) -> -0.1
        _ -> 0
  in
     velocity + direction

move : {keys| w: Bool, s: Bool} -> Vec3 -> Vec3
move k position =
  let
    direction =
      case (k.w, k.s) of
        (True, False) -> 0.1
        (False, True) -> -0.1
        _ -> 0
  in
     position `add` (vec3 0 0 direction)


main : Program Never
main =
  Html.program
    { init = init
    , view = view
    , subscriptions = subscriptions
    , update = update
    }

subscriptions : Model -> Sub Action
subscriptions _ =
  [ AnimationFrame.diffs Animate
  , Keyboard.downs (keyChange True)
  , Keyboard.ups (keyChange False)
  ]
  |> Sub.batch

keyChange : Bool -> Keyboard.KeyCode -> Action
keyChange on keyCode =
  (case keyCode of
    37 -> \k -> {k | left = on}
    39 -> \k -> {k | right = on}
    38 -> \k -> {k | up = on}
    40 -> \k -> {k | down = on}
    87 -> \k -> {k | w = on}
    83 -> \k -> {k | s = on}
    _ -> Basics.identity
  ) |> KeyChange

-- MESHES

starMesh : Drawable { position:Vec3, coord:Vec3 }
starMesh =
  Triangle
    [ ( { position = vec3 1 1 0, coord = vec3 1 1 0 }
      , { position = vec3 -1 1 0, coord = vec3 0 1 0 }
      , { position = vec3 1 -1 0, coord = vec3 1 0 0 }
      ),
      ( { position = vec3 -1 1 0, coord = vec3 0 1 0 }
      , { position = vec3 1 -1 0, coord = vec3 1 0 0 }
      , { position = vec3 -1 -1 0, coord = vec3 0 0 0 }
      )
    ]

-- VIEW

view : Model -> Html Action
view {texture, position, rx, ry, stars} =
  let
    entities = List.concat (List.map (renderStar rx ry texture position) stars)
  in
    div
      []
      [ WebGL.toHtml
          [ width 500, height 500, style [("backgroundColor", "black")]  ]
          entities
      , div
          [ style
              [ ("position", "absolute")
              , ("font-family", "monospace")
              , ("text-align", "center")
              , ("left", "20px")
              , ("right", "20px")
              , ("top", "500px")
              ]
          ]
          [ text message]
      ]

renderStar : Float -> Float -> Maybe Texture -> Vec3 -> Star -> List Renderable
renderStar rx ry texture position star =
  renderEntity starMesh rx ry texture position star

message : String
message =
    "Keys are: F -> change texture mode, Right/Left/Up/Down rotate, w/s -> move camera in/out"


renderEntity : Drawable { position:Vec3, coord:Vec3 } -> Float -> Float -> Maybe Texture -> Vec3 -> Star -> List Renderable
renderEntity mesh rx ry texture position star =
  case texture of
    Nothing ->
     []

    Just tex ->
     [renderWithConfig [Enable Blend, Disable DepthTest, BlendFunc (SrcAlpha, One)] vertexShader fragmentShader mesh (uniformsStar rx rx tex star)]

uniformsStar : Float -> Float -> Texture -> Star -> { texture:Texture, perspective:Mat4, camera:Mat4, worldSpace: Mat4 }
uniformsStar rx ry texture { dist, rotationSpeed, color, angle } =
  { texture = texture
  , worldSpace = makeRotate ry (vec3 1 0 0) `mul`  makeRotate rx (vec3 0 1 0) `mul`  makeRotate angle (vec3 0 0 1) `mul` makeTranslate (vec3 dist 0 0)
  , camera = makeLookAt (vec3 0 0 0) (vec3 0 0 -4) (vec3 0 1 0)
  , perspective = makePerspective 45 1 0.01 100
  }

-- SHADERS

vertexShader : Shader { attr| position:Vec3, coord:Vec3 } { unif | worldSpace:Mat4, perspective:Mat4, camera:Mat4 } { vcoord:Vec2 }
vertexShader = [glsl|

  precision mediump float;
  attribute vec3 position;
  attribute vec3 coord;
  uniform mat4 worldSpace;
  uniform mat4 perspective;
  uniform mat4 camera;
  varying vec2 vcoord;

  void main() {
    gl_Position = perspective * camera * worldSpace* vec4(position, 1.0);
    vcoord = coord.xy;
  }
|]

fragmentShader : Shader {} { unif | texture:Texture } { vcoord:Vec2 }
fragmentShader = [glsl|
  precision mediump float;
  uniform sampler2D texture;
  varying vec2 vcoord;

  void main () {
      gl_FragColor = texture2D(texture, vcoord);
  }

|]
