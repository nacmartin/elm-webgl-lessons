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
  , thetaX : Float
  , thetaY : Float
  , keys : Keys
  , position: Vec3
  , rx: Float
  , ry: Float
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
  , thetaX = 0
  , thetaY = 0
  , rx = 0
  , ry = 0
  , keys = Keys False False False False False False
  , position = (vec3 0 0 -4)
  }
  , fetchTexture |> Task.perform TexturesError TexturesLoaded
  )

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
        | thetaX = model.thetaX + model.rx * dt / 1000
        , thetaY = model.thetaY + model.ry * dt / 1000
        , position = model.position
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

cube : Drawable { position:Vec3, coord:Vec3 }
cube =
  Triangle <|
  List.concatMap rotatedFace [ (0,0), (90,0), (180,0), (270,0), (0,90), (0,-90) ]


rotatedFace : (Float,Float) -> List ({ position:Vec3, coord:Vec3 }, { position:Vec3, coord:Vec3 }, { position:Vec3, coord:Vec3 })
rotatedFace (angleX,angleY) =
  let
    x = makeRotate (degrees angleX) (vec3 1 0 0)
    y = makeRotate (degrees angleY) (vec3 0 1 0)
    t = x `mul` y `mul` makeTranslate (vec3 0 0 1)
    each f (a,b,c) =
      (f a, f b, f c)
  in
    List.map (each (\x -> {x | position = transform t x.position })) face

face : List ({ position:Vec3, coord:Vec3 }, { position:Vec3, coord:Vec3 }, { position:Vec3, coord:Vec3 })
face =
  let
    topLeft     = { position = vec3 -1  1 0, coord = vec3 0 1 0 }
    topRight    = { position = vec3  1  1 0, coord = vec3 1 1 0 }
    bottomLeft  = { position = vec3 -1 -1 0, coord = vec3 0 0 0 }
    bottomRight = { position = vec3  1 -1 0, coord = vec3 1 0 0 }
  in
    [ (topLeft,topRight,bottomLeft)
    , (bottomLeft,topRight,bottomRight)
    ]

-- VIEW

view : Model -> Html Action
view {texture, thetaX, thetaY, position, rx, ry} =
  let
    entities = renderEntity cube thetaX thetaY texture position
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

message : String
message =
    "Keys are: F -> change texture mode, Right/Left/Up/Down rotate, w/s -> move camera in/out"


renderEntity : Drawable { position:Vec3, coord:Vec3 } -> Float -> Float -> Maybe Texture -> Vec3 -> List Renderable
renderEntity mesh thetaX thetaY texture position =
  case texture of
    Nothing ->
     []

    Just tex ->
     [render vertexShader fragmentShader mesh (uniformsCube thetaX thetaY tex position)]

uniformsCube : Float -> Float -> Texture -> Vec3 -> { texture:Texture, rotation:Mat4, perspective:Mat4, camera:Mat4, displacement: Vec3 }
uniformsCube tx ty texture displacement =
    let --a = Debug.log "a" (makeRotate ty (vec3 1 0 0) `mul`  makeRotate tx (vec3 0 1 0) `mul`  makeRotate 0 (vec3 0 0 1))
        c = Debug.log "camera" (makeLookAt displacement (displacement `add` k) (vec3 0 1 0))
        d = Debug.log "displacement" displacement
    in
        { texture = texture
        , rotation = makeRotate ty (vec3 1 0 0) `mul`  makeRotate tx (vec3 0 1 0) `mul`  makeRotate 0 (vec3 0 0 1)
        , perspective = makePerspective 45 1 0.01 100
        , camera = makeLookAt displacement (displacement `add` k) (vec3 0 1 0)
        , displacement = (vec3 0 0 0)
        }

-- SHADERS

vertexShader : Shader { attr| position:Vec3, coord:Vec3 } { unif | rotation:Mat4, displacement:Vec3, perspective:Mat4, camera:Mat4 } { vcoord:Vec2 }
vertexShader = [glsl|

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

fragmentShader : Shader {} { unif | texture:Texture } { vcoord:Vec2 }
fragmentShader = [glsl|
  precision mediump float;
  uniform sampler2D texture;
  varying vec2 vcoord;

  void main () {
      gl_FragColor = texture2D(texture, vcoord);
  }

|]
