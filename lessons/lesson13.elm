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
import AnimationFrame

type alias Model =
  { textures : (Maybe Texture, Maybe Texture)
  , positionMoon: Vec3
  , positionCrate: Vec3
  , lighting: Bool
  , theta : Float
  , mouseStatus : MouseStatus
  , pointColourText: {x:String, y:String, z:String}
  , pointColour: Vec3
  , ambientColourText: {x:String, y:String, z:String}
  , ambientColour: Vec3
  , pointText: {x:String, y:String, z:String}
  , point: Vec3
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
  | TexturesLoaded (Maybe Texture, Maybe Texture)
  | MouseChange (Model -> Model)
  | UseLighting
  | ChangePointColourR String
  | ChangePointColourG String
  | ChangePointColourB String
  | ChangeAmbientColourR String
  | ChangeAmbientColourG String
  | ChangeAmbientColourB String
  | ChangePointX String
  | ChangePointY String
  | ChangePointZ String
  | Animate Time

init : (Model, Cmd Action)
init =
  ( {textures = (Nothing, Nothing)
  , theta = 0
  , positionCrate = (vec3 -2 0 0)
  , positionMoon = (vec3 2 0 0)
  , lighting = True
  , mouseStatus = MouseStatus False 0 0 0 0
  , pointColourText = {x="1", y="1", z="1"}
  , pointColour = (vec3 1 1 1)
  , ambientColourText = {x="0.2", y="0.2", z="0.2"}
  , ambientColour = (vec3 0.2 0.2 0.2)
  , pointText = {x="0", y="0", z="0"}
  , point = (vec3 0 0 0)
  }
  , fetchTextures |> Task.perform TexturesError TexturesLoaded
  )

fetchTextures : Task Error (Maybe Texture, Maybe Texture)
fetchTextures =
  loadTexture "textures/moon.gif" `Task.andThen` \moonTexture ->
  loadTexture "textures/crate.gif" `Task.andThen` \crateTexture ->
  Task.succeed (Just moonTexture, Just crateTexture)

update : Action -> Model -> (Model, Cmd Action)
update action model =
  case action of
    TexturesError err ->
      (model, Cmd.none)
    TexturesLoaded textures ->
      ({model | textures = textures}, Cmd.none)
    MouseChange mousefunc ->
      (mousefunc model, Cmd.none )
    UseLighting ->
        ( { model | lighting = not model.lighting }, Cmd.none )
    ChangePointX value ->
        let
            (numeric, textual) = updateAndParseX model.point model.pointText value
        in
           ( { model | point = numeric, pointText = textual }, Cmd.none )
    ChangePointY value ->
        let
            (numeric, textual) = updateAndParseY model.point model.pointText value
        in
           ( { model | point = numeric, pointText = textual }, Cmd.none )
    ChangePointZ value ->
        let
            (numeric, textual) = updateAndParseZ model.point model.pointText value
        in
           ( { model | point = numeric, pointText = textual }, Cmd.none )
    ChangePointColourR value ->
        let
            (numeric, textual) = updateAndParseX model.pointColour model.pointColourText value
        in
           ( { model | pointColour = numeric, pointColourText = textual }, Cmd.none )
    ChangePointColourG value ->
        let
            (numeric, textual) = updateAndParseY model.pointColour model.pointColourText value
        in
           ( { model | pointColour = numeric, pointColourText = textual }, Cmd.none )
    ChangePointColourB value ->
        let
            (numeric, textual) = updateAndParseZ model.pointColour model.pointColourText value
        in
           ( { model | pointColour = numeric, pointColourText = textual }, Cmd.none )
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
    Animate dt ->
      ( { model | theta = model.theta + dt / 1000 }
        , Cmd.none
      )

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
  [ AnimationFrame.diffs Animate
  ]
  |> Sub.batch

-- MESHES

numSegments = 20

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
    List.concatMap (\(longitude1, longitude2) -> sphereFace latitude1 latitude2 longitude1 longitude2 radius) longitudes


sphereFace : Float -> Float -> Float -> Float -> Float -> List ({ position:Vec3, coord:Vec3, norm:Vec3 }, { position:Vec3, coord:Vec3, norm:Vec3 }, { position:Vec3, coord:Vec3, norm:Vec3 })
sphereFace latitude1 latitude2 longitude1 longitude2 radius =
  let
    theta1 = degrees (180 * latitude1)
    theta2 = degrees (180 * latitude2)
    phi1 = degrees (360 * longitude1)
    phi2 = degrees (360 * longitude2)
    topLeft     = { position = vec3 ((cos theta2) * (sin phi1) * radius) ((sin theta2) * radius) ((cos theta2) * (cos phi1) * radius), coord = vec3 (longitude1-0.5) (latitude2-0.5) 0, norm = (vec3 ((cos theta2) * (sin phi1)) ((sin theta2)) ((cos theta2) * (cos phi1))) }
    topRight    = { position = vec3 ((cos theta2) * (sin phi2) * radius) ((sin theta2) * radius) ((cos theta2) * (cos phi2) * radius), coord = vec3 (longitude2-0.5) (latitude2-0.5) 0, norm = (vec3 ((cos theta2) * (sin phi2)) ((sin theta2)) ((cos theta2) * (cos phi2))) }
    bottomLeft  = { position = vec3 ((cos theta1) * (sin phi1) * radius) ((sin theta1) * radius) ((cos theta1) * (cos phi1) * radius), coord = vec3 (longitude1-0.5) (latitude1-0.5) 0, norm = (vec3 ((cos theta1) * (sin phi1)) ((sin theta1)) ((cos theta1) * (cos phi1))) }
    bottomRight = { position = vec3 ((cos theta1) * (sin phi2) * radius) ((sin theta1) * radius) ((cos theta1) * (cos phi2) * radius), coord = vec3 (longitude2-0.5) (latitude1-0.5) 0, norm =  (vec3 ((cos theta1) * (sin phi2)) ((sin theta1)) ((cos theta1) * (cos phi2))) }
  in
    [ (topLeft,topRight,bottomLeft)
    , (bottomLeft,topRight,bottomRight)
    ]

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
    normal = (normalize(transform t (vec3 0 0 1)) )
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
view {textures, theta, positionCrate, positionMoon, lighting, pointColour, point, ambientColour, pointColourText, ambientColourText, pointText} =
  let
    ( textureMoon, textureCrate ) = textures
    entities = renderEntity sphere theta textureMoon positionMoon lighting pointColour point ambientColour
              ++ renderEntity cube theta textureCrate positionCrate lighting pointColour point ambientColour
  in
    div
      []
      [ WebGL.toHtml
        [ width 600, height 600, style [("backgroundColor", "black")]  ]
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
          [ h2 [] [ text "Point Light"]
          , div []
            [ text "Position: "
            , text " x: "
            , input [type' "text", step "0.01", onInput ChangePointX, value pointText.x] []
            , text " y: "
            , input [type' "text", step "0.01", onInput ChangePointY, value pointText.y] []
            , text " z: "
            , input [type' "text", step "0.01", onInput ChangePointZ, value pointText.z] []
            ]
          , div []
            [ text "Colour: "
            , text " R: "
            , input [type' "text", step "0.01", onInput ChangePointColourR, value pointColourText.x] []
            , text " G: "
            , input [type' "text", step "0.01", onInput ChangePointColourG, value pointColourText.y] []
            , text " B: "
            , input [type' "text", step "0.01", onInput ChangePointColourB, value pointColourText.z] []
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
        ]
      ]



renderEntity : Drawable { position:Vec3, coord:Vec3, norm: Vec3 } -> Float -> Maybe Texture -> Vec3 -> Bool -> Vec3 -> Vec3 -> Vec3 -> List Renderable
renderEntity mesh theta texture position lighting pointColour point ambientColour =
  case texture of
    Nothing ->
     []

    Just tex ->
     [render vertexShader fragmentShader mesh (uniformsShpere theta tex position lighting pointColour point ambientColour)]

uniformsShpere : Float -> Texture -> Vec3 -> Bool -> Vec3 -> Vec3 -> Vec3 -> { texture:Texture, worldSpace:Mat4, perspective:Mat4, camera:Mat4, normalMatrix: Mat4, lighting: Bool, pointColour: Vec3, ambientColour: Vec3, point: Vec3 }
uniformsShpere tx texture displacement lighting pointColour point ambientColour=
  let worldSpace = (translate displacement (makeRotate tx (vec3 0 1 0) ))
      camera = makeLookAt (vec3 0 0 10) (vec3 0 0 -1) (vec3 0 1 0)
      perspective = makePerspective 45 1 0.1 100
  in
    { texture = texture
    , worldSpace = worldSpace
    , perspective = perspective
    , camera = camera
    , normalMatrix = transpose(inverseOrthonormal( worldSpace ))
    , lighting = lighting
    , pointColour = pointColour
    , ambientColour = ambientColour
    , point = point
    }

-- SHADERS

vertexShader : Shader { attr| position:Vec3, coord:Vec3, norm:Vec3 } { unif | worldSpace:Mat4, perspective:Mat4, camera:Mat4, normalMatrix:Mat4 } { vcoord:Vec2, vPosition:Vec3, vTransformedNormal:Vec3 }
vertexShader = [glsl|

  precision mediump float;

  attribute vec3 position;
  attribute vec3 coord;
  attribute vec3 norm;

  uniform mat4 worldSpace;
  uniform mat4 perspective;
  uniform mat4 normalMatrix;
  uniform mat4 camera;

  varying vec2 vcoord;
  varying vec3 vPosition;
  varying vec3 vTransformedNormal;

  void main() {
    vcoord = coord.xy;
    vTransformedNormal = vec3(normalMatrix * vec4(norm, 0.0));
    vec4 v4Position =  worldSpace * vec4(position, 1.0);
    gl_Position = perspective * camera * v4Position;
    vPosition =  vec3(v4Position);
  }
|]

fragmentShader : Shader {} { unif | texture:Texture, lighting:Bool, ambientColour:Vec3, pointColour:Vec3, point:Vec3 } { vcoord:Vec2, vPosition:Vec3, vTransformedNormal:Vec3 }
fragmentShader = [glsl|
  precision mediump float;

  uniform sampler2D texture;
  uniform bool lighting;
  uniform vec3 ambientColour;
  uniform vec3 pointColour;
  uniform vec3 point;

  varying vec2 vcoord;
  varying vec3 vPosition;
  varying vec3 vTransformedNormal;

  void main () {
      vec3 lightWeighting;
      lightWeighting = vec3(1.0, 1.0, 1.0);
      if (!lighting) {
        lightWeighting = vec3(1.0, 1.0, 1.0);
      } else {
        vec3 lightDirection = normalize(point - vPosition );

        float directionalLightWeighting = max(dot(normalize(vTransformedNormal), lightDirection), 0.0);
        lightWeighting = ambientColour + pointColour * directionalLightWeighting;
      }
      vec4 fragmentColor;
      fragmentColor = texture2D(texture, vec2(vcoord.s, vcoord.t));
      gl_FragColor = vec4(fragmentColor.rgb * lightWeighting, fragmentColor.a);
  }


|]

--varying vec2 vTextureCoord;
--  varying vec3 vTransformedNormal;
--  varying vec4 vPosition;
--
--  uniform bool uUseLighting;
--  uniform bool uUseTextures;
--
--  uniform vec3 uAmbientColor;
--
--  uniform vec3 uPointLightingLocation;
--  uniform vec3 uPointLightingColor;
--
--  uniform sampler2D uSampler;
--
--  void main(void) {
--    vec3 lightWeighting;
--    if (!uUseLighting) {
--      lightWeighting = vec3(1.0, 1.0, 1.0);
--    } else {
--      vec3 lightDirection = normalize(uPointLightingLocation - vPosition.xyz);
--
--      float directionalLightWeighting = max(dot(normalize(vTransformedNormal), lightDirection), 0.0);
--      lightWeighting = uAmbientColor + uPointLightingColor * directionalLightWeighting;
--    }
--
--    vec4 fragmentColor;
--    if (uUseTextures) {
--      fragmentColor = texture2D(uSampler, vec2(vTextureCoord.s, vTextureCoord.t));
--    } else {
--      fragmentColor = vec4(1.0, 1.0, 1.0, 1.0);
--    }
--    gl_FragColor = vec4(fragmentColor.rgb * lightWeighting, fragmentColor.a);
--  }
