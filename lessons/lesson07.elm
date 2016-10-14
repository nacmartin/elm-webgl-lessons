import Debug
import Keyboard
import String exposing (toFloat)
import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (..)
import Math.Matrix4 exposing (..)
import Task exposing (Task)
import Time exposing (Time)
import WebGL exposing (..)
import Html exposing (Html, text, div, input, h2)
import Html.App as Html
import Html.Events exposing (onInput, onClick)
import AnimationFrame
import Html.Attributes exposing (width, height, style, type', checked, step, value)

type alias Model =
  { texture : Maybe Texture
  , thetaX : Float
  , thetaY : Float
  , keys : Keys
  , position: Vec3
  , rx: Float
  , ry: Float
  , lighting: Bool
  , directionalColourText: {x:String, y:String, z:String}
  , directionalColour: Vec3
  , ambientColourText: {x:String, y:String, z:String}
  , ambientColour: Vec3
  , directionalText: {x:String, y:String, z:String}
  , directional: Vec3
  }

type alias Triplet =
  { x : String
  , y : String
  , z : String
  }

type Action
  = TexturesError Error
  | TexturesLoaded (Maybe Texture)
  | TextureChange (Model -> Model)
  | KeyChange (Keys -> Keys)
  | Animate Time
  | UseLighting
  | ChangeDirectionalColourR String
  | ChangeDirectionalColourG String
  | ChangeDirectionalColourB String
  | ChangeAmbientColourR String
  | ChangeAmbientColourG String
  | ChangeAmbientColourB String
  | ChangeDirectionalX String
  | ChangeDirectionalY String
  | ChangeDirectionalZ String

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
  , lighting = True
  , directionalColourText = {x="0.8", y="0.2", z="0.2"}
  , directionalColour = (vec3 0.8 0.2 0.2)
  , ambientColourText = {x="0.2", y="0.2", z="0.9"}
  , ambientColour = (vec3 0.2 0.2 0.8)
  , directionalText = {x="-0.25", y="0.25", z="-1"}
  , directional = (vec3 -0.25 -0.25 -1)
  }
  , fetchTextures |> Task.perform TexturesError TexturesLoaded
  )

fetchTextures : Task Error (Maybe Texture)
fetchTextures =
  loadTextureWithFilter WebGL.Nearest "textures/crate.gif" `Task.andThen` \nearestTexture ->
  Task.succeed (Just nearestTexture)

update : Action -> Model -> (Model, Cmd Action)
update action model =
  case action of
    TexturesError err ->
      (model, Cmd.none)
    TexturesLoaded texture ->
      ({model | texture = texture}, Cmd.none)
    TextureChange keyfunc ->
      ((keyfunc model), Cmd.none)
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
    UseLighting ->
        ( { model | lighting = not model.lighting }, Cmd.none )
    ChangeDirectionalX value ->
        let
            (numeric, textual) = updateAndParseX model.directional model.directionalText value
        in
           ( { model | directional = numeric, directionalText = textual }, Cmd.none )
    ChangeDirectionalY value ->
        let
            (numeric, textual) = updateAndParseY model.directional model.directionalText value
        in
           ( { model | directional = numeric, directionalText = textual }, Cmd.none )
    ChangeDirectionalZ value ->
        let
            (numeric, textual) = updateAndParseZ model.directional model.directionalText value
        in
           ( { model | directional = numeric, directionalText = textual }, Cmd.none )
    ChangeDirectionalColourR value ->
        let
            (numeric, textual) = updateAndParseX model.directionalColour model.directionalColourText value
        in
           ( { model | directionalColour = numeric, directionalColourText = textual }, Cmd.none )
    ChangeDirectionalColourG value ->
        let
            (numeric, textual) = updateAndParseY model.directionalColour model.directionalColourText value
        in
           ( { model | directionalColour = numeric, directionalColourText = textual }, Cmd.none )
    ChangeDirectionalColourB value ->
        let
            (numeric, textual) = updateAndParseZ model.directionalColour model.directionalColourText value
        in
           ( { model | directionalColour = numeric, directionalColourText = textual }, Cmd.none )
    ChangeAmbientColourR value ->
        let
            (numeric, textual) = updateAndParseX model.ambientColour model.ambientColourText value
        in
           ( { model | ambientColour = numeric, ambientColourText = textual }, Cmd.none )
    ChangeAmbientColourG value ->
        let
            (numeric, textual) = updateAndParseY model.ambientColour model.ambientColourText value
        in
           ( { model | ambientColour = numeric, ambientColourText = textual }, Cmd.none )
    ChangeAmbientColourB value ->
        let
            (numeric, textual) = updateAndParseZ model.ambientColour model.ambientColourText value
        in
           ( { model | ambientColour = numeric, ambientColourText = textual }, Cmd.none )

updateAndParseX:  Vec3 -> Triplet -> String -> (Vec3, Triplet)
updateAndParseX default textual value =
  let text = { textual | x = value }
  in
    updateAndParse default text

updateAndParseY:  Vec3 -> Triplet -> String -> (Vec3, Triplet)
updateAndParseY default textual value =
  let text = { textual | y = value }
  in
    updateAndParse default text

updateAndParseZ:  Vec3 -> Triplet -> String -> (Vec3, Triplet)
updateAndParseZ default textual value =
  let text = { textual | z = value }
  in
    updateAndParse default text

updateAndParse: Vec3 -> Triplet -> (Vec3, Triplet)
updateAndParse default text =
  case (String.toFloat text.x, String.toFloat text.y, String.toFloat text.z) of
      (Ok vr, Ok vg, Ok vb) -> (vec3 vr vg vb, text)
      _ -> ( default, text )

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

cube : Drawable { position:Vec3, coord:Vec3, norm:Vec3 }
cube =
  Triangle <|
  List.concatMap rotatedFace [ (0,0), (90,0), (180,0), (270,0), (0,90), (0,-90) ]


rotatedFace : (Float,Float) -> List ({ position:Vec3, coord:Vec3, norm:Vec3  }, { position:Vec3, coord:Vec3, norm:Vec3  }, { position:Vec3, coord:Vec3, norm:Vec3  })
rotatedFace (angleX,angleY) =
  let
    x = makeRotate (degrees angleX) (vec3 1 0 0)
    y = makeRotate (degrees angleY) (vec3 0 1 0)
    t = x `mul` y `mul` makeTranslate (vec3 0 0 1)
    normal = Math.Vector3.negate (normalize(transform t (vec3 0 0 1)) )
    each f (a,b,c) =
      (f a, f b, f c)
  in
    List.map (each (\x -> {x | position = transform t x.position, norm = normal })) face

face : List ({ position:Vec3, coord:Vec3, norm:Vec3 }, { position:Vec3, coord:Vec3, norm:Vec3 }, { position:Vec3, coord:Vec3, norm:Vec3 })
face =
  let
    topLeft     = { position = vec3 -1  1 0, coord = vec3 0 1 0, norm = vec3 0 0 1 }
    topRight    = { position = vec3  1  1 0, coord = vec3 1 1 0, norm = vec3 0 0 1 }
    bottomLeft  = { position = vec3 -1 -1 0, coord = vec3 0 0 0, norm = vec3 0 0 1 }
    bottomRight = { position = vec3  1 -1 0, coord = vec3 1 0 0, norm = vec3 0 0 1 }
  in
    [ (topLeft,topRight,bottomLeft)
    , (bottomLeft,topRight,bottomRight)
    ]

-- VIEW

view : Model -> Html Action
view {texture, thetaX, thetaY, position, rx, ry, lighting, directionalColour, directional, ambientColour, directionalColourText, ambientColourText, directionalText} =
  let
    entities = renderEntity cube thetaX thetaY texture position lighting directionalColour directional ambientColour
  in
    div
      []
      [ WebGL.toHtml
        [ width 400, height 400, style [("backgroundColor", "black")]  ]
        entities
      , div
        [ style
          [ ("left", "20px")
          , ("right", "20px")
          , ("top", "500px")
          ]
        ]
        [ div []
          [ input [type' "checkbox", onClick UseLighting, checked lighting] []
          , text " Use lighting"
          ]
        , div []
          [ h2 [] [ text "Directional Light"]
          , div []
            [ text "Direction: "
            , text " x: "
            , input [type' "text", step "0.01", onInput ChangeDirectionalX, value directionalText.x] []
            , text " y: "
            , input [type' "text", step "0.01", onInput ChangeDirectionalY, value directionalText.y] []
            , text " z: "
            , input [type' "text", step "0.01", onInput ChangeDirectionalZ, value directionalText.z] []
            ]
          , div []
            [ text "Colour: "
            , text " R: "
            , input [type' "text", step "0.01", onInput ChangeDirectionalColourR, value directionalColourText.x] []
            , text " G: "
            , input [type' "text", step "0.01", onInput ChangeDirectionalColourG, value directionalColourText.y] []
            , text " B: "
            , input [type' "text", step "0.01", onInput ChangeDirectionalColourB, value directionalColourText.z] []
            ]
          , h2 [] [ text "Ambient Light"]
          , div []
            [ text "Colour: "
            , text " R: "
            , input [type' "text", step "0.01", onInput ChangeAmbientColourR, value ambientColourText.x] []
            , text " G: "
            , input [type' "text", step "0.01", onInput ChangeAmbientColourG, value ambientColourText.y] []
            , text " B: "
            , input [type' "text", step "0.01", onInput ChangeAmbientColourB, value ambientColourText.z] []
            ]
          ]
        , text message
        ]
      ]

message : String
message =
    "Keys are: Right/Left/Up/Down rotate, w/s -> move camera in/out"


renderEntity : Drawable { position:Vec3, coord:Vec3, norm: Vec3 } -> Float -> Float -> Maybe Texture -> Vec3 -> Bool -> Vec3 -> Vec3 -> Vec3 -> List Renderable
renderEntity mesh thetaX thetaY texture position lighting directionalColour directional ambientColour =
  case texture of
    Nothing ->
     []

    Just tex ->
     [render vertexShader fragmentShader mesh (uniformsCube thetaX thetaY tex position lighting directionalColour directional ambientColour)]

uniformsCube : Float -> Float -> Texture -> Vec3 -> Bool -> Vec3 -> Vec3 -> Vec3 -> { texture:Texture, worldSpace:Mat4, perspective:Mat4, camera:Mat4, normalMatrix: Mat4, lighting: Bool, directionalColour: Vec3, ambientColour: Vec3, directional: Vec3 }
uniformsCube tx ty texture displacement lighting directionalColour directional ambientColour=
  let worldSpace = (rotate tx (vec3 0 1 0) (rotate ty (vec3 1 0 0) (makeTranslate displacement)))
      camera = makeLookAt (vec3 0 0 0) (vec3 0 0 -4) (vec3 0 1 0)
      perspective = makePerspective 45 1 0.1 100

  in
    { texture = texture
    , worldSpace = worldSpace
    , perspective = perspective
    , camera = camera
    , normalMatrix = transpose(inverseOrthonormal( worldSpace `mul` camera))
    , lighting = lighting
    , directionalColour = directionalColour
    , ambientColour = ambientColour
    , directional = directional
    }

-- SHADERS

vertexShader : Shader { attr| position:Vec3, coord:Vec3, norm:Vec3 } { unif | worldSpace:Mat4, perspective:Mat4, camera:Mat4, normalMatrix:Mat4, lighting:Bool, directionalColour:Vec3, ambientColour:Vec3, directional: Vec3 } { vcoord:Vec2, lightWeighting:Vec3 }
vertexShader = [glsl|

  precision mediump float;

  attribute vec3 position;
  attribute vec3 coord;
  attribute vec3 norm;

  uniform mat4 worldSpace;
  uniform mat4 perspective;
  uniform mat4 normalMatrix;
  uniform mat4 camera;
  uniform bool lighting;
  uniform vec3 directionalColour;
  uniform vec3 directional;
  uniform vec3 ambientColour;

  varying vec2 vcoord;
  varying vec3 lightWeighting;

  void main() {
    gl_Position = perspective * camera * worldSpace * vec4(position, 1.0);
    vcoord = coord.xy;

    if (!lighting) {
      lightWeighting = vec3(1.0, 1.0, 1.0);
    } else {
      vec4 transformedNormal = normalMatrix * vec4(norm, 0.0);
      float directionalLightWeighting = max(dot(transformedNormal, vec4(directional, 0)), 0.0);
      lightWeighting = ambientColour + directionalColour * directionalLightWeighting;
    }
  }
|]

fragmentShader : Shader {} { unif | texture:Texture } { vcoord:Vec2, lightWeighting:Vec3 }
fragmentShader = [glsl|
  precision mediump float;

  uniform sampler2D texture;
  varying vec2 vcoord;
  varying vec3 lightWeighting;

  void main () {
      vec4 textureColor = texture2D(texture, vec2(vcoord.s, vcoord.t));
      gl_FragColor = vec4(textureColor.rgb * lightWeighting, textureColor.a);
  }

|]
