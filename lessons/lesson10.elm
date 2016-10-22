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
import Html.Attributes exposing (width, height, style)
import List.Extra exposing (getAt, greedyGroupsOf)

effectiveFPMS = 30.0 / 10000.0

type alias Vertex = { position : Vec3, coord: Vec3 }

type alias Model =
  { texture : Maybe Texture
  , keys : Keys
  , position: Vec3
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
  }

init : (Model, Cmd Action)
init =
  ( {texture = Nothing
  , position = vec3 0 2 10
  , keys = Keys False False False False False False
  , world = Triangle []
  }
  , Cmd.batch[ fetchTexture |> Task.perform TexturesError TexturesLoaded
             , fetchWorld
             ]
  )

fetchWorld : Cmd Action
fetchWorld =
  let
    url =
      "/meshes/world10.txt"
  in
    Task.perform FetchFail FetchSucceed (Http.getString url `Task.andThen` decodeWorld)

match : List String -> List (List Regex.Match)
match lines =
    List.map (\x -> Regex.find Regex.All (Regex.regex "-?\\d+\\.\\d+") x) lines

filter5 : List (List a) -> List (List a)
filter5 lines = List.filter (\x -> 5 == List.length x) lines

toFloatSure : String -> Float
toFloatSure str =
  case String.toFloat str of
    Ok num -> num
    Err _ -> 0.0

extractMatches : List (List Regex.Match) -> List (List Float)
extractMatches matches = List.map (\lineMatch -> List.map (toFloatSure << .match) lineMatch) matches

makeTriangle : List (List Float) ->
           ( Vertex
           , Vertex
           , Vertex
           )
makeTriangle vertexes =
  case vertexes of
      a :: b :: c :: [] -> ( makeVertex a, makeVertex b, makeVertex c)
      _ -> ( Vertex (vec3 0 0 0) (vec3 0 0 1)
           , Vertex (vec3 0 0 0) (vec3 0 0 1)
           , Vertex (vec3 0 0 0) (vec3 0 0 1)
           )

makeVertex : List Float -> Vertex
makeVertex coords =
  case coords of
      a :: b :: c :: d :: e :: [] -> { position = vec3 a b c, coord = vec3 d e 0 }
      _ -> { position = vec3 0 0 0, coord = vec3 0 0 0 }

decodeWorld : String -> Task Http.Error (Drawable Vertex)
decodeWorld source =
  succeed (Triangle (List.map makeTriangle (greedyGroupsOf 3 (extractMatches (filter5 (match (String.lines source)))))))
  --[ ( { position = vec3 -1 1 0, coord = vec3 0 1 0 }
  --  , { position = vec3 1 1 0, coord = vec3 1 1 0 }
  --  , { position = vec3 -1 -1 0, coord = vec3 0 0 0 }
  --  )])

-- decodeWorld : String -> Task Http.Error (Drawable { position:Vec3, coord:Vec3 })
-- decodeWorld source =
--     let
--         a = Debug.log "a" source
--     in
--        succeed (Triangle
--        [ ( { position = vec3 -1 1 0, coord = vec3 0 1 0 }
--          , { position = vec3 1 1 0, coord = vec3 1 1 0 }
--          , { position = vec3 -1 -1 0, coord = vec3 0 0 0 }
--          )])

fetchTexture : Task Error (Maybe Texture)
fetchTexture =
  loadTextureWithFilter WebGL.Linear "textures/crate.gif" `Task.andThen` \linearTexture ->
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
            |> move model.keys
        }
        , Cmd.none
      )


rotateX : {keys| up: Bool, down: Bool} -> Float -> Float
rotateX k velocity =
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
    38 -> \k -> {k | up = on}
    40 -> \k -> {k | down = on}
    87 -> \k -> {k | w = on}
    83 -> \k -> {k | s = on}
    _ -> Basics.identity
  ) |> KeyChange

-- VIEW

view : Model -> Html Action
view {texture, world} =
  div
    []
    [ WebGL.toHtml
        [ width 500, height 500, style [("backgroundColor", "black")]  ]
        (renderEntity world texture)
    , div
        [ style
            [ ("font-family", "monospace")
            , ("left", "20px")
            , ("right", "20px")
            , ("top", "500px")
            ]
        ]
        [ text message]
    ]

message : String
message =
    "Up/Down rotate, w/s -> move camera in/out"

renderEntity : Drawable { position:Vec3, coord:Vec3 } -> Maybe Texture -> List Renderable
renderEntity world texture =
  case texture of
    Nothing ->
     []
    Just tex ->
     [render vertexShader fragmentShader world (uniforms tex)]

uniforms : Texture -> { texture:Texture, perspective:Mat4, camera:Mat4, worldSpace: Mat4 }
uniforms texture =
  { texture = texture
  , worldSpace = makeTranslate (vec3 0 -0.5 1)
  , camera = makeLookAt (vec3 0 0 -4) (vec3 0 0 -1) (vec3 0 1 0)
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
