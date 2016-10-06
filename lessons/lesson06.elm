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
  }


type Action
  = TexturesError Error
  | TexturesLoaded (Maybe Texture, Maybe Texture)
  | Animate Time

init : (Model, Cmd Action)
init =
  ( {textures = (Nothing, Nothing), theta = 0, textureSelected = 2}
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
    Animate dt ->
      ({model | theta = model.theta + dt / 1000}, Cmd.none)

main : Program Never
main =
  Html.program
    { init = init
    , view = view
    , subscriptions = (\model -> AnimationFrame.diffs Animate)
    , update = update
    }

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
view {textures, theta, textureSelected} =
  let 
    (texture1, texture2) = textures
    tex = if textureSelected == 1 then texture1 else texture2
  in
    renderEntity cube theta tex
    |> WebGL.toHtml [ width 400, height 400 ]

renderEntity : Drawable { position:Vec3, coord:Vec3 } -> Float -> Maybe Texture -> List Renderable
renderEntity mesh theta texture =
  case texture of
    Nothing ->
     []

    Just tex ->
     [render vertexShader fragmentShader mesh (uniformsCube theta tex)]

uniformsCube : Float -> Texture -> { texture:Texture, rotation:Mat4, perspective:Mat4, camera:Mat4, displacement:Vec3 }
uniformsCube t texture =
  { texture = texture
  , rotation = makeRotate t (vec3 1 0 0) `mul`  makeRotate t (vec3 0 1 0) `mul`  makeRotate t (vec3 0 0 1)
  , perspective = makePerspective 45 1 0.01 100
  , camera = makeLookAt (vec3 0 0 5) (vec3 0 0 0) (vec3 0 1 0)
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
