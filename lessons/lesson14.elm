import Debug
import Keyboard
import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (..)
import Math.Matrix4 exposing (..)
import Task exposing (Task, succeed)
import Time exposing (Time)
import WebGL exposing (..)
import Html exposing (Html, text, div)
import Html.App as Html
import AnimationFrame
import Random
import Http
import Array
import String
import Regex
import Json.Decode exposing (..)
import Html.Attributes exposing (width, height, style)
import List.Extra exposing (getAt, greedyGroupsOf)

effectiveFPMS = 30.0 / 10000.0

type alias Vertex = { position : Vec3, coord: Vec3, normal: Vec3 }

type alias Model =
  { texture : Maybe Texture
  , keys : Keys
  , position: Vec3
  , facing: Vec3
  , upwardsAngle: Float
  , joggingPhase: Float
  , world: Drawable Vertex
  }

type Action
  = TexturesError Error
  | TexturesLoaded (Maybe Texture)
  | KeyChange (Keys -> Keys)
  | Animate Time
  | FetchFail Http.Error
  | FetchSucceed (Drawable Vertex )

type alias Keys =
  { left : Bool
  , right : Bool
  , up : Bool
  , down : Bool
  , w : Bool
  , s : Bool
  , a : Bool
  , d : Bool
  }

init : (Model, Cmd Action)
init =
  ( {texture = Nothing
  , position = vec3 0 0.5 10
  , facing = vec3 0 0 -1
  , joggingPhase = 0
  , upwardsAngle = 0
  , keys = Keys False False False False False False False False
  , world = Triangle []
  }
  , Cmd.batch[ fetchTexture |> Task.perform TexturesError TexturesLoaded
             , fetchWorld
             ]
  )

fetchWorld : Cmd Action
fetchWorld =
  let
    uri =
      "meshes/Teapot.json"
  in
    Task.perform FetchFail FetchSucceed (Http.get worldDecoder uri `Task.andThen` getWorld)


type alias World = { vertexPositions : List Float, vertexNormals : List Float, vertexTextureCoords : List Float, indices : List Int}

makeVec3 : List Float -> Vec3
makeVec3 coords =
  case coords of
      a :: b :: [] -> vec3 a b 0
      a :: b :: c :: [] -> vec3 a b c
      _ -> vec3 0 0 0

--getWorld : World -> Task Http.Error (Drawable)
getWorld { vertexPositions, vertexNormals, vertexTextureCoords, indices} =
  let
    vertexes = List.map3 (\a b c -> { position = makeVec3 a, coord = makeVec3 b, normal = makeVec3 c}) (greedyGroupsOf 3 vertexPositions) (greedyGroupsOf 2 vertexTextureCoords) (greedyGroupsOf 2 vertexNormals)
    a =  Debug.log "a" (List.length vertexes)
    b =  Debug.log "b" (List.length indices)
    n =  Debug.log "n" (List.length vertexNormals)
    p =  Debug.log "p" (List.length vertexPositions)
    c =  Debug.log "c" (List.length vertexTextureCoords)
  in
    Task.succeed (WebGL.IndexedTriangles ( List.reverse vertexes, indices))
    -- Task.succeed   (WebGL.IndexedTriangles
    -- ( [ { position = vec3 -1 1 1, coord = vec3 0 0 0, normal = vec3 0 0 0 }
    --   , { position = vec3 1 1 1, coord = vec3 0 0 0, normal = vec3 0 0 0 }
    --   , { position = vec3 1 1 -1, coord = vec3 0 0 0, normal = vec3 0 0 0 }
    --   , { position = vec3 -1 1 -1, coord = vec3 0 0 0, normal = vec3 0 0 0 }
    --   , { position = vec3 0 -1 0, coord = vec3 0 0 0, normal = vec3 0 0 0 }
    --   ]
    -- , [ 0, 1, 3
    --   , 1, 2, 3
    --   , 3, 2, 4
    --   , 2, 1, 4
    --   , 0, 1, 4
    --   , 3, 0, 4]
    -- ))

worldDecoder = object4 World ("vertexPositions" := list float) ("vertexTextureCoords" := list float) ("vertexTextureCoords" := list float) ("indices" := list int)

fetchTexture : Task Error (Maybe Texture)
fetchTexture =
  loadTextureWithFilter WebGL.Linear "textures/metal.jpg" `Task.andThen` \linearTexture ->
  Task.succeed (Just linearTexture)

update : Action -> Model -> (Model, Cmd Action)
update action model =
  case action of
    FetchFail err ->
      (model, Cmd.none)
    FetchSucceed world ->
      ({model | world = world}, Cmd.none)
    TexturesError err ->
      (model, Cmd.none)
    TexturesLoaded texture ->
      ({model | texture = texture}, Cmd.none)
    KeyChange keyfunc ->
      ({model | keys = keyfunc model.keys}, Cmd.none)
    Animate dt ->
      ( { model
        | position = model.position
          |> move (dt/500) model.keys model.facing model.joggingPhase
        , facing = model.facing
          |> rotateY dt model.keys
        , upwardsAngle = model.upwardsAngle
          |> rotateX dt model.keys
        , joggingPhase = model.joggingPhase
          |> updateJoggingPhase dt model.keys
        }
        , Cmd.none
      )

updateJoggingPhase : Float -> {keys| a: Bool, s: Bool, w: Bool, d: Bool} -> Float -> Float
updateJoggingPhase dt k phase =
  case (k.a, k.s, k.d, k.w) of
    (False, False, False, False) -> phase
    (True, True, True, True) -> phase
    (True, True, False, False) -> phase
    (False, False, True, True) -> phase
    _ -> phase + dt/100

rotateX : Float -> {keys| up: Bool, down: Bool} -> Float -> Float
rotateX dt k upwardsAngle =
  let
    direction =
      case (k.up, k.down) of
        (True, False) -> 1
        (False, True) -> -1
        _ -> 0
  in
    addWithCap 89 -89 upwardsAngle (direction * dt/10)

addWithCap : Float -> Float -> Float -> Float -> Float
addWithCap max min value toAdd =
  if value >= max && toAdd > 0 then max
  else if value <= min && toAdd < 0 then min
  else value + toAdd

rotateY : Float -> {keys| left: Bool, right: Bool} -> Vec3 -> Vec3
rotateY dt k facing =
  let
    direction =
      case (k.left, k.right) of
        (True, False) -> 1
        (False, True) -> -1
        _ -> 0
  in
     transform (makeRotate (direction * dt/1000) j) facing

move : Float -> {keys| w: Bool, s: Bool, a: Bool, d: Bool} -> Vec3 -> Float -> Vec3-> Vec3
move dt k facing joggingPhase position =
  let
    forward =
      case (k.w, k.s) of
        (True, False) -> 1 * dt
        (False, True) -> -1 * dt
        _ -> 0
    strafe =
      case (k.a, k.d) of
        (True, False) -> -2 * dt
        (False, True) -> 2 * dt
        _ -> 0
  in
     setY (0.5 + (sin joggingPhase) * 0.05) (position `add` (Math.Vector3.scale strafe (facing `cross` j)) `add` (Math.Vector3.scale forward facing) )


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
    38 -> \k -> {k | up = on}
    40 -> \k -> {k | down = on}
    39 -> \k -> {k | right = on}
    37 -> \k -> {k | left = on}
    87 -> \k -> {k | w = on}
    83 -> \k -> {k | s = on}
    65 -> \k -> {k | a = on}
    68 -> \k -> {k | d = on}
    _ -> Basics.identity
  ) |> KeyChange

-- VIEW

view : Model -> Html Action
view { texture, world, position, facing, upwardsAngle } =
  let a = Debug.log "a" world
  in
    div
      []
      [ WebGL.toHtml
          [ width 500, height 500, style [("backgroundColor", "black")]  ]
          ( renderEntity world texture position facing upwardsAngle)
      , div
          [ style
              [ ("font-family", "monospace")
              , ("left", "20px")
              , ("right", "20px")
              , ("top", "500px")
              ]
          ]
          [ text message ]
      ]

message : String
message =
    "Up/Down/Left/Right turn head, w/a/s/d -> move around"

renderEntity : Drawable { position:Vec3, coord:Vec3, normal:Vec3 } -> Maybe Texture -> Vec3 -> Vec3 -> Float -> List Renderable
renderEntity world texture position facing upwardsAngle =
  case texture of
    Nothing ->
     []
    Just tex ->
     [render vertexShader fragmentShader world (uniforms tex position facing upwardsAngle)]

uniforms : Texture -> Vec3 -> Vec3 -> Float -> { texture:Texture, perspective:Mat4, camera:Mat4, worldSpace: Mat4 }
uniforms texture position facing upwardsAngle =
  { texture = texture
  , worldSpace = makeTranslate (vec3 0 0 0)
  , camera = makeLookAt position (position `add` (transform (makeRotate (degrees upwardsAngle) (facing `cross` j)) facing)) j
  , perspective = makePerspective 45 1 0.01 100
  }

-- SHADERS

vertexShader : Shader { attr| position:Vec3, coord:Vec3 } { unif | worldSpace:Mat4, perspective:Mat4, camera:Mat4} { vcoord:Vec2 }
vertexShader = [glsl|

  precision mediump float;
  attribute vec3 position;
  attribute vec3 coord;
  uniform mat4 worldSpace;
  uniform mat4 perspective;
  uniform mat4 camera;
  varying vec2 vcoord;

  void main() {
    gl_Position = perspective * camera * worldSpace * vec4(position, 1.0);
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
