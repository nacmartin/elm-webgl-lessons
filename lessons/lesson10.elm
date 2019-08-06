module Main exposing (main)

import Array
import Browser
import Browser.Events exposing (onAnimationFrameDelta, onKeyDown, onKeyUp)
import Html exposing (Html, div, text)
import Html.Attributes exposing (height, style, width)
import Html.Events exposing (keyCode)
import Http
import Json.Decode as Decode
import List.Extra exposing (greedyGroupsOf)
import Math.Matrix4 exposing (..)
import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (..)
import Random
import Regex
import String
import Task exposing (Task, succeed)
import WebGL exposing (Entity, Mesh, Shader)
import WebGL.Texture as Texture exposing (Error, Texture, defaultOptions)


type alias Vertex =
    { position : Vec3, coord : Vec3 }


type alias Model =
    { texture : Maybe Texture
    , keys : Keys
    , position : Vec3
    , facing : Vec3
    , upwardsAngle : Float
    , joggingPhase : Float
    , world : Mesh Vertex
    }


type Msg
    = TextureLoaded (Result Error Texture)
    | KeyChange Bool Int
    | Animate Float
    | WorldLoaded (Result Http.Error (Mesh Vertex))


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


init : ( Model, Cmd Msg )
init =
    ( { texture = Nothing
      , position = vec3 0 0.5 1
      , facing = vec3 0 0 -1
      , joggingPhase = 0
      , upwardsAngle = 0
      , keys = Keys False False False False False False False False
      , world = WebGL.triangles []
      }
    , Cmd.batch
        [ Task.attempt TextureLoaded fetchTexture
        , Http.get
            { url = "meshes/world10.txt"
            , expect = Http.expectString (Result.map decodeWorld >> WorldLoaded)
            }
        ]
    )


match : List String -> List (List Regex.Match)
match lines =
    List.map
        (Regex.find
            (Maybe.withDefault Regex.never (Regex.fromString "-?\\d+\\.\\d+"))
        )
        lines


filter5 : List (List a) -> List (List a)
filter5 lines =
    List.filter (\x -> 5 == List.length x) lines


toFloatSure : String -> Float
toFloatSure str =
    case String.toFloat str of
        Just num ->
            num

        Nothing ->
            0.0


extractMatches : List (List Regex.Match) -> List (List Float)
extractMatches matches =
    List.map (\lineMatch -> List.map (toFloatSure << .match) lineMatch) matches


makeWebGLTriangle :
    List (List Float)
    -> ( Vertex, Vertex, Vertex )
makeWebGLTriangle vertexes =
    case vertexes of
        a :: b :: c :: [] ->
            ( makeVertex a, makeVertex b, makeVertex c )

        _ ->
            ( Vertex (vec3 0 0 0) (vec3 0 0 1)
            , Vertex (vec3 0 0 0) (vec3 0 0 1)
            , Vertex (vec3 0 0 0) (vec3 0 0 1)
            )


makeVertex : List Float -> Vertex
makeVertex coords =
    case coords of
        a :: b :: c :: d :: e :: [] ->
            { position = vec3 a b c, coord = vec3 d e 0 }

        _ ->
            { position = vec3 0 0 0, coord = vec3 0 0 0 }


decodeWorld : String -> Mesh Vertex
decodeWorld source =
    WebGL.triangles (List.map makeWebGLTriangle (greedyGroupsOf 3 (extractMatches (filter5 (match (String.lines source))))))


fetchTexture : Task Error Texture
fetchTexture =
    Texture.loadWith { defaultOptions | minify = Texture.linear, magnify = Texture.linear } "textures/crate.gif"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        WorldLoaded world ->
            ( { model | world = Result.withDefault model.world world }, Cmd.none )

        TextureLoaded texture ->
            ( { model | texture = Result.toMaybe texture }, Cmd.none )

        KeyChange on key ->
            ( { model | keys = keyChange on key model.keys }, Cmd.none )

        Animate dt ->
            ( { model
                | position =
                    model.position
                        |> move (dt / 500) model.keys model.facing model.joggingPhase
                , facing =
                    model.facing
                        |> rotateY dt model.keys
                , upwardsAngle =
                    model.upwardsAngle
                        |> rotateX dt model.keys
                , joggingPhase =
                    model.joggingPhase
                        |> updateJoggingPhase dt model.keys
              }
            , Cmd.none
            )


type T4 a b c d
    = T4 a b c d


updateJoggingPhase : Float -> { keys | a : Bool, s : Bool, w : Bool, d : Bool } -> Float -> Float
updateJoggingPhase dt k phase =
    case T4 k.a k.s k.d k.w of
        T4 False False False False ->
            phase

        T4 True True True True ->
            phase

        T4 True True False False ->
            phase

        T4 False False True True ->
            phase

        _ ->
            phase + dt / 100


rotateX : Float -> { keys | up : Bool, down : Bool } -> Float -> Float
rotateX dt k upwardsAngle =
    let
        direction =
            case ( k.up, k.down ) of
                ( True, False ) ->
                    1

                ( False, True ) ->
                    -1

                _ ->
                    0
    in
    addWithCap 89 -89 upwardsAngle (direction * dt / 10)


addWithCap : Float -> Float -> Float -> Float -> Float
addWithCap max min value toAdd =
    if value >= max && toAdd > 0 then
        max

    else if value <= min && toAdd < 0 then
        min

    else
        value + toAdd


rotateY : Float -> { keys | left : Bool, right : Bool } -> Vec3 -> Vec3
rotateY dt k facing =
    let
        direction =
            case ( k.left, k.right ) of
                ( True, False ) ->
                    1

                ( False, True ) ->
                    -1

                _ ->
                    0
    in
    transform (makeRotate (direction * dt / 1000) j) facing


move : Float -> { keys | w : Bool, s : Bool, a : Bool, d : Bool } -> Vec3 -> Float -> Vec3 -> Vec3
move dt k facing joggingPhase position =
    let
        forward =
            case ( k.w, k.s ) of
                ( True, False ) ->
                    1 * dt

                ( False, True ) ->
                    -1 * dt

                _ ->
                    0

        strafe =
            case ( k.a, k.d ) of
                ( True, False ) ->
                    -1 * dt

                ( False, True ) ->
                    1 * dt

                _ ->
                    0
    in
    setY (0.5 + sin joggingPhase * 0.05)
        (position
            |> add (Math.Vector3.scale strafe (cross facing j))
            |> add (Math.Vector3.scale forward facing)
        )


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> init
        , view = view
        , subscriptions = subscriptions
        , update = update
        }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ onAnimationFrameDelta Animate
        , onKeyDown (Decode.map (KeyChange True) keyCode)
        , onKeyUp (Decode.map (KeyChange False) keyCode)
        ]


keyChange : Bool -> Int -> Keys -> Keys
keyChange on keyCode k =
    case keyCode of
        38 ->
            { k | up = on }

        40 ->
            { k | down = on }

        39 ->
            { k | right = on }

        37 ->
            { k | left = on }

        87 ->
            { k | w = on }

        83 ->
            { k | s = on }

        65 ->
            { k | a = on }

        68 ->
            { k | d = on }

        _ ->
            k



-- VIEW


view : Model -> Html Msg
view { texture, world, position, facing, upwardsAngle } =
    div
        []
        [ WebGL.toHtml
            [ width 500, height 500, style "background" "black" ]
            (renderEntity world texture position facing upwardsAngle)
        , div
            [ style "font-family" "monospace"
            , style "left" "20px"
            , style "right" "20px"
            , style "top" "500px"
            ]
            [ text message ]
        ]


message : String
message =
    "Up/Down/Left/Right turn head, w/a/s/d -> move around"


renderEntity : Mesh { position : Vec3, coord : Vec3 } -> Maybe Texture -> Vec3 -> Vec3 -> Float -> List Entity
renderEntity world texture position facing upwardsAngle =
    case texture of
        Nothing ->
            []

        Just tex ->
            [ WebGL.entity vertexShader fragmentShader world (uniforms tex position facing upwardsAngle) ]


uniforms : Texture -> Vec3 -> Vec3 -> Float -> { texture : Texture, perspective : Mat4, camera : Mat4, worldSpace : Mat4 }
uniforms texture position facing upwardsAngle =
    { texture = texture
    , worldSpace = makeTranslate (vec3 0 0 0)
    , camera = makeLookAt position (add position (transform (makeRotate (degrees upwardsAngle) (cross facing j)) facing)) j
    , perspective = makePerspective 45 1 0.01 100
    }



-- SHADERS


vertexShader : Shader { attr | position : Vec3, coord : Vec3 } { unif | worldSpace : Mat4, perspective : Mat4, camera : Mat4 } { vcoord : Vec2 }
vertexShader =
    [glsl|

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


fragmentShader : Shader {} { unif | texture : Texture } { vcoord : Vec2 }
fragmentShader =
    [glsl|
  precision mediump float;
  uniform sampler2D texture;
  varying vec2 vcoord;

  void main () {
      gl_FragColor = texture2D(texture, vcoord);
  }

|]
