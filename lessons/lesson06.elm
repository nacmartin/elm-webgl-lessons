import Debug
import Keyboard
import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (..)
import Math.Matrix4 exposing (..)
import Task exposing (Task)
import Time exposing (Time)
import WebGL exposing (..)
import Html exposing (Html)
import Html.App as Html
import AnimationFrame
import Html.Attributes exposing (width, height)

type alias Model =
  { textures : (Maybe Texture, Maybe Texture)
  , theta : Float
  , textureSelected: Int
  , keys : Keys
  , position: Vec3
  }


type Action
  = TexturesError Error
  | TexturesLoaded (Maybe Texture, Maybe Texture)
  | TextureChange (Model -> Model)
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
  ( {textures = (Nothing, Nothing)
  , theta = 0
  , textureSelected = 1
  , keys = Keys False False False False False False
  , position = (vec3 0 0 -4)
  }
  , fetchTextures |> Task.perform TexturesError TexturesLoaded
  )

fetchTextures : Task Error (Maybe Texture, Maybe Texture)
fetchTextures =
  loadTextureWithFilter Linear "textures/crate.gif" `Task.andThen` \linearTexture ->
  loadTextureWithFilter Nearest "textures/crate.gif" `Task.andThen` \nearestTexture ->
  Task.succeed (Just linearTexture, Just nearestTexture)

update : Action -> Model -> (Model, Cmd Action)
update action model =
  case action of
    TexturesError err ->
      (model, Cmd.none)
    TexturesLoaded textures ->
      ({model | textures = textures}, Cmd.none)
    TextureChange keyfunc ->
      ((keyfunc model), Cmd.none)
    KeyChange keyfunc ->
      ({model | keys = keyfunc model.keys}, Cmd.none)
    Animate dt ->
      ( { model
        | theta = model.theta + dt / 1000
        , position = model.position
            |> move model.keys
        }
        , Cmd.none
      )

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
  , Keyboard.downs textureChange
  ]
  |> Sub.batch

textureChange : Keyboard.KeyCode -> Action
textureChange keyCode =
  (case keyCode of
    70 -> \m -> if m.textureSelected == 1 then {m | textureSelected = 2} else {m| textureSelected = 1}
    _ -> Basics.identity
  ) |> TextureChange

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
view {textures, theta, textureSelected, position} =
  let
    (texture1, texture2) = textures
    tex = if textureSelected == 1 then texture1 else texture2
  in
    renderEntity cube theta tex position
    |> WebGL.toHtml [ width 400, height 400 ]

renderEntity : Drawable { position:Vec3, coord:Vec3 } -> Float -> Maybe Texture -> Vec3 -> List Renderable
renderEntity mesh theta texture position =
  case texture of
    Nothing ->
     []

    Just tex ->
     [render vertexShader fragmentShader mesh (uniformsCube theta tex position)]

uniformsCube : Float -> Texture -> Vec3 -> { texture:Texture, rotation:Mat4, perspective:Mat4, camera:Mat4, displacement: Vec3 }
uniformsCube t texture displacement =
  { texture = texture
  , rotation = makeRotate t (vec3 1 0 0) `mul`  makeRotate t (vec3 0 1 0) `mul`  makeRotate t (vec3 0 0 1)
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