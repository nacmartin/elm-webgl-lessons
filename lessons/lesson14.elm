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
  , position: Vec3
  , world: Drawable Vertex
  , useLighting: Bool
  , useSpecular: Bool
  , ambientColourText: {x:String, y:String, z:String}
  , ambientColour: Vec3
  , pointText: {x:String, y:String, z:String}
  , point: Vec3
  , pointColourText: {x:String, y:String, z:String}
  , pointColour: Vec3
  , theta : Float
  }

type Action
  = TexturesError Error
  | TexturesLoaded (Maybe Texture)
  | Animate Time
  | FetchFail Http.Error
  | FetchSucceed (Drawable Vertex )

init : (Model, Cmd Action)
init =
  ( {texture = Nothing
  , position = vec3 0 0 0
  , world = Triangle []
  , useLighting = True
  , useSpecular = True
  , pointColourText = {x="1", y="1", z="1"}
  , ambientColour = (vec3 0.5 0.5 0.5)
  , pointText = {x="0", y="0", z="0"}
  , point = (vec3 -10 4 20)
  , pointColour = (vec3 1 1 1)
  , ambientColourText = {x="0.2", y="0.2", z="0.2"}
  , theta = 0
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
    vertexes = List.map3 (\a b c -> { position = makeVec3 a, coord = makeVec3 b, normal = makeVec3 c}) (greedyGroupsOf 3 vertexPositions) (greedyGroupsOf 2 vertexTextureCoords) (greedyGroupsOf 3 vertexNormals)
  in
--    Task.succeed (sphere)
    Task.succeed (WebGL.IndexedTriangles ( vertexes, indices))

worldDecoder = object4 World ("vertexPositions" := list float) ("vertexNormals" := list float) ("vertexTextureCoords" := list float) ("indices" := list int)

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
    Animate dt ->
      ( { model | theta = model.theta + dt / 1000 }
        , Cmd.none
      )

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

-- VIEW

view : Model -> Html Action
view { texture, theta, world, position, useLighting, useSpecular, pointColour, point, ambientColour } =
  div
    []
    [ WebGL.toHtml
        [ width 500, height 500, style [("backgroundColor", "black")]  ]
        ( renderEntity world theta texture position useLighting useSpecular pointColour point ambientColour )
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

renderEntity : Drawable { position:Vec3, coord:Vec3, normal:Vec3 } -> Float -> Maybe Texture -> Vec3 -> Bool -> Bool -> Vec3 -> Vec3 -> Vec3 -> List Renderable
renderEntity world theta texture position useLighting useSpecular pointColour point ambientColour  =
  case texture of
    Nothing ->
     []
    Just tex ->
     [render vertexShader fragmentShader world (uniforms tex theta position useLighting useSpecular pointColour point ambientColour )]

uniforms : Texture -> Float -> Vec3 -> Bool -> Bool -> Vec3 -> Vec3 -> Vec3 -> { texture:Texture, perspective:Mat4, camera:Mat4, worldSpace: Mat4, useLighting: Bool, normalMatrix: Mat4, useSpecular: Bool, pointColour: Vec3, ambientColour: Vec3, point: Vec3}
uniforms texture theta position useLighting useSpecular pointColour point ambientColour =
  let
    worldSpace = (translate position (rotate (degrees 23.4) (vec3 1 0 1) (makeRotate theta (vec3 0 1 0) )))
    camera = makeLookAt (vec3 0 0 50) (vec3 0 0 -1) (vec3 0 1 0)
    perspective = makePerspective 45 1 0.1 100
  in
    { texture = texture
    , worldSpace = worldSpace
    , camera = camera
    , perspective = perspective
    , normalMatrix = transpose(inverseOrthonormal( worldSpace ))
    , useLighting = useLighting
    , useSpecular = useSpecular
    , pointColour = pointColour
    , ambientColour = ambientColour
    , point = point
    }

-- SHADERS
vertexShader : Shader { attr| position:Vec3, coord:Vec3, normal:Vec3 } { unif | worldSpace:Mat4, perspective:Mat4, camera:Mat4, normalMatrix:Mat4 } { vcoord:Vec2, vPosition:Vec3, vTransformedNormal:Vec3 }
vertexShader = [glsl|

  precision mediump float;

  attribute vec3 position;
  attribute vec3 coord;
  attribute vec3 normal;

  uniform mat4 worldSpace;
  uniform mat4 perspective;
  uniform mat4 normalMatrix;
  uniform mat4 camera;

  varying vec2 vcoord;
  varying vec3 vPosition;
  varying vec3 vTransformedNormal;

  void main() {
    vec4 v4Position = camera * worldSpace * vec4(position, 1.0);
    gl_Position = perspective * v4Position;
    vPosition =  vec3(v4Position);
    vcoord = coord.xy;
    vTransformedNormal = vec3(normalMatrix * vec4(normal, 0.0));
  }
|]

fragmentShader : Shader {} { unif | texture:Texture, useSpecular: Bool, useLighting:Bool, ambientColour:Vec3, pointColour:Vec3, point:Vec3 } { vcoord:Vec2, vPosition:Vec3, vTransformedNormal:Vec3 }
fragmentShader = [glsl|
  precision mediump float;

  uniform sampler2D texture;
  uniform bool useLighting;
  uniform bool useSpecular;
  uniform vec3 ambientColour;
  uniform vec3 pointColour;
  uniform vec3 point;

  varying vec2 vcoord;
  varying vec3 vPosition;
  varying vec3 vTransformedNormal;

  void main () {
      vec3 lightWeighting;
      lightWeighting = vec3(1.0, 1.0, 1.0);
      if (!useLighting) {
        lightWeighting = vec3(1.0, 1.0, 1.0);
      } else {

        vec3 lightDirection = normalize(point - vPosition);
        vec3 normal = normalize(vTransformedNormal);

        float specularLightWeighting = 0.0;
        if (useSpecular) {
            vec3 eyeDirection = normalize(-vPosition.xyz);
            vec3 reflectionDirection = reflect(-lightDirection, normal);

            specularLightWeighting = pow(max(dot(reflectionDirection, eyeDirection), 0.0), 32.0);
        }

        float diffuseLightWeighting = max(dot(normal, lightDirection), 0.0);
        lightWeighting = ambientColour
            + pointColour * specularLightWeighting
            + ambientColour * diffuseLightWeighting;
      }
      vec4 fragmentColor;
      fragmentColor = texture2D(texture, vec2(vcoord.s, vcoord.t));
      gl_FragColor = vec4(fragmentColor.rgb * lightWeighting, fragmentColor.a);
  }


|]
