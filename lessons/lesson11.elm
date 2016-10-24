import Debug
import Mouse
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
import Html.Attributes exposing (width, height, style, type', checked, step, value)

type alias Model =
  { texture : Maybe Texture
  , position: Vec3
  , lighting: Bool
  , thetaX : Float
  , thetaY : Float
  , mouseStatus : MouseStatus
  , directionalColourText: {x:String, y:String, z:String}
  , directionalColour: Vec3
  , ambientColourText: {x:String, y:String, z:String}
  , ambientColour: Vec3
  , directionalText: {x:String, y:String, z:String}
  , directional: Vec3
  }

type alias MouseStatus =
  { pressed : Bool
  , x1 : Int
  , y1 : Int
  , x2 : Int
  , y2 : Int
  }

type alias Triplet =
  { x : String
  , y : String
  , z : String
  }

type Action
  = TexturesError Error
  | TexturesLoaded (Maybe Texture)
  | MouseChange (Model -> Model)
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

init : (Model, Cmd Action)
init =
  ( {texture = Nothing
  , thetaX = 0
  , thetaY = 0
  , position = (vec3 0 0 -4)
  , lighting = True
  , mouseStatus = MouseStatus False 0 0 0 0
  , directionalColourText = {x="1", y="1", z="1"}
  , directionalColour = (vec3 1 1 1)
  , ambientColourText = {x="0.2", y="0.2", z="0.2"}
  , ambientColour = (vec3 0.2 0.2 0.2)
  , directionalText = {x="1", y="1", z="1"}
  , directional = (vec3 1 1 1)
  }
  , fetchTextures |> Task.perform TexturesError TexturesLoaded
  )

fetchTextures : Task Error (Maybe Texture)
fetchTextures =
  loadTexture "textures/moon.gif" `Task.andThen` \nearestTexture ->
  Task.succeed (Just nearestTexture)

update : Action -> Model -> (Model, Cmd Action)
update action model =
  case action of
    TexturesError err ->
      (model, Cmd.none)
    TexturesLoaded texture ->
      ({model | texture = texture}, Cmd.none)
    MouseChange mousefunc ->
      (mousefunc model, Cmd.none )
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

rotateX : MouseStatus -> Float -> Float
rotateX m thetaX =
  case m.pressed of
    True -> thetaX + degrees(Basics.toFloat(m.x2 - m.x1))
    False -> thetaX

rotateY : MouseStatus -> Float -> Float
rotateY m thetaY =
  case m.pressed of
    True -> thetaY + degrees(Basics.toFloat(m.y2 - m.y1))
    False -> thetaY

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
  [ Mouse.downs mouseDown
  , Mouse.ups mouseUp
  , Mouse.moves mouseMove
  ]
  |> Sub.batch

mouseDown : Mouse.Position -> Action
mouseDown {x, y} =
  let
    changeMouseStatus = (\x y ms -> {ms| x1 = x, y1 = y, x2 = x, y2 = y, pressed = True})
  in
    MouseChange (\m -> { m| mouseStatus = changeMouseStatus x y m.mouseStatus })

mouseMove : Mouse.Position -> Action
mouseMove {x, y} =
  let
    changeMouseStatus = (\x y ms -> {ms| x1 = x, y1 = y, x2 = x, y2 = y, pressed = True})
  in
    MouseChange (\m ->
        case m.mouseStatus.pressed of
            False -> m
            True -> { m
                        | mouseStatus = changeMouseStatus x y m.mouseStatus
                        , thetaX = m.thetaX + degrees (Basics.toFloat(x - m.mouseStatus.x1))
                        , thetaY = m.thetaY + degrees (Basics.toFloat(y - m.mouseStatus.y1))
                        })

mouseUp : Mouse.Position -> Action
mouseUp {x, y} =
  let
    changeMouseStatus = (\ms -> {ms| pressed = False})
  in
  MouseChange (\m -> { m| mouseStatus = changeMouseStatus m.mouseStatus})

-- MESHES

numSegments = 30

sphere : Drawable { position:Vec3, coord:Vec3, norm:Vec3 }
sphere =
  let
    latitudes = List.map (\idx -> (idx/numSegments, (idx + 1)/numSegments)) [(-numSegments/2)..(numSegments/2) - 1]
  in
    Triangle <|
    List.concatMap (\(lat1, lat2) -> ring lat1 lat2 numSegments 1) latitudes

ring : Float -> Float -> Float -> Float -> List ({ position:Vec3, coord:Vec3, norm:Vec3 }, { position:Vec3, coord:Vec3, norm:Vec3 }, { position:Vec3, coord:Vec3, norm:Vec3 })
ring latitude1 latitude2 segments radius =
  let
    longitudes = List.map (\idx -> ( idx/segments, (idx + 1)/segments)) [0..segments - 1]
  in
    List.concatMap (\(longitude1, longitude2) -> face latitude1 latitude2 longitude1 longitude2 radius) longitudes


face : Float -> Float -> Float -> Float -> Float -> List ({ position:Vec3, coord:Vec3, norm:Vec3 }, { position:Vec3, coord:Vec3, norm:Vec3 }, { position:Vec3, coord:Vec3, norm:Vec3 })
face latitude1 latitude2 longitude1 longitude2 radius =
  let
    theta1 = degrees (180 * latitude1)
    theta2 = degrees (180 * latitude2)
    phi1 = degrees (360 * longitude1)
    phi2 = degrees (360 * longitude2)
    topLeft     = { position = vec3 ((cos theta2) * (sin phi1) * radius) ((sin theta2) * radius) ((cos theta2) * (cos phi1) * radius), coord = vec3 (longitude1-0.5) (latitude2-0.5) 0, norm = vec3 ((cos theta2) * (sin phi1)) ((sin theta2)) ((cos theta2) * (cos phi1)) }
    topRight    = { position = vec3 ((cos theta2) * (sin phi2) * radius) ((sin theta2) * radius) ((cos theta2) * (cos phi2) * radius), coord = vec3 (longitude2-0.5) (latitude2-0.5) 0, norm = vec3 ((cos theta2) * (sin phi2)) ((sin theta2)) ((cos theta2) * (cos phi2)) }
    bottomLeft  = { position = vec3 ((cos theta1) * (sin phi1) * radius) ((sin theta1) * radius) ((cos theta1) * (cos phi1) * radius), coord = vec3 (longitude1-0.5) (latitude1-0.5) 0, norm = vec3 ((cos theta1) * (sin phi1)) ((sin theta1)) ((cos theta1) * (cos phi1)) }
    bottomRight = { position = vec3 ((cos theta1) * (sin phi2) * radius) ((sin theta1) * radius) ((cos theta1) * (cos phi2) * radius), coord = vec3 (longitude2-0.5) (latitude1-0.5) 0, norm = vec3 ((cos theta1) * (sin phi2)) ((sin theta1)) ((cos theta1) * (cos phi2)) }
  in
    [ (topLeft,topRight,bottomLeft)
    , (bottomLeft,topRight,bottomRight)
    ]

-- VIEW

view : Model -> Html Action
view {texture, thetaX, thetaY, position, lighting, directionalColour, directional, ambientColour, directionalColourText, ambientColourText, directionalText} =
  let
    entities = renderEntity sphere thetaX thetaY texture position lighting directionalColour directional ambientColour
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
    "Move the Moon dragging the mouse"


renderEntity : Drawable { position:Vec3, coord:Vec3, norm: Vec3 } -> Float -> Float -> Maybe Texture -> Vec3 -> Bool -> Vec3 -> Vec3 -> Vec3 -> List Renderable
renderEntity mesh thetaX thetaY texture position lighting directionalColour directional ambientColour =
  case texture of
    Nothing ->
     []

    Just tex ->
     [render vertexShader fragmentShader mesh (uniformsShpere thetaX thetaY tex position lighting directionalColour directional ambientColour)]

uniformsShpere : Float -> Float -> Texture -> Vec3 -> Bool -> Vec3 -> Vec3 -> Vec3 -> { texture:Texture, worldSpace:Mat4, perspective:Mat4, camera:Mat4, normalMatrix: Mat4, lighting: Bool, directionalColour: Vec3, ambientColour: Vec3, directional: Vec3 }
uniformsShpere tx ty texture displacement lighting directionalColour directional ambientColour=
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
