module Main exposing (main)

import Browser
import Browser.Events exposing (onMouseDown, onMouseMove, onMouseUp)
import Html exposing (Html, div, h2, input, text)
import Html.Attributes exposing (checked, height, step, style, type_, value, width)
import Html.Events exposing (onClick, onInput)
import Json.Decode as Decode exposing (Decoder)
import Math.Matrix4 exposing (..)
import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (..)
import String
import Task exposing (Task)
import WebGL exposing (Entity, Mesh, Shader)
import WebGL.Texture as Texture exposing (Error, Texture)


type alias Model =
    { texture : Maybe Texture
    , position : Vec3
    , lighting : Bool
    , thetaX : Float
    , thetaY : Float
    , mouseStatus : MouseStatus
    , directionalColourText : { x : String, y : String, z : String }
    , directionalColour : Vec3
    , ambientColourText : { x : String, y : String, z : String }
    , ambientColour : Vec3
    , directionalText : { x : String, y : String, z : String }
    , directional : Vec3
    }


type alias MouseStatus =
    { pressed : Bool
    , x : Int
    , y : Int
    }


type alias Triplet =
    { x : String
    , y : String
    , z : String
    }


type Msg
    = TextureLoaded (Result Error Texture)
    | MouseDown { x : Int, y : Int }
    | MouseMove { x : Int, y : Int }
    | MouseUp { x : Int, y : Int }
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


init : ( Model, Cmd Msg )
init =
    ( { texture = Nothing
      , thetaX = 0
      , thetaY = 0
      , position = vec3 0 0 -4
      , lighting = True
      , mouseStatus = MouseStatus False 0 0
      , directionalColourText = { x = "1", y = "1", z = "1" }
      , directionalColour = vec3 1 1 1
      , ambientColourText = { x = "0.2", y = "0.2", z = "0.2" }
      , ambientColour = vec3 0.2 0.2 0.2
      , directionalText = { x = "1", y = "1", z = "1" }
      , directional = vec3 1 1 1
      }
    , Task.attempt TextureLoaded (Texture.load "textures/moon.gif")
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TextureLoaded texture ->
            ( { model | texture = Result.toMaybe texture }, Cmd.none )

        MouseDown pos ->
            ( mouseDown pos model, Cmd.none )

        MouseMove pos ->
            ( mouseMove pos model, Cmd.none )

        MouseUp pos ->
            ( mouseUp pos model, Cmd.none )

        UseLighting ->
            ( { model | lighting = not model.lighting }, Cmd.none )

        ChangeDirectionalX value ->
            let
                ( numeric, textual ) =
                    updateAndParseX model.directional model.directionalText value
            in
            ( { model | directional = numeric, directionalText = textual }, Cmd.none )

        ChangeDirectionalY value ->
            let
                ( numeric, textual ) =
                    updateAndParseY model.directional model.directionalText value
            in
            ( { model | directional = numeric, directionalText = textual }, Cmd.none )

        ChangeDirectionalZ value ->
            let
                ( numeric, textual ) =
                    updateAndParseZ model.directional model.directionalText value
            in
            ( { model | directional = numeric, directionalText = textual }, Cmd.none )

        ChangeDirectionalColourR value ->
            let
                ( numeric, textual ) =
                    updateAndParseX model.directionalColour model.directionalColourText value
            in
            ( { model | directionalColour = numeric, directionalColourText = textual }, Cmd.none )

        ChangeDirectionalColourG value ->
            let
                ( numeric, textual ) =
                    updateAndParseY model.directionalColour model.directionalColourText value
            in
            ( { model | directionalColour = numeric, directionalColourText = textual }, Cmd.none )

        ChangeDirectionalColourB value ->
            let
                ( numeric, textual ) =
                    updateAndParseZ model.directionalColour model.directionalColourText value
            in
            ( { model | directionalColour = numeric, directionalColourText = textual }, Cmd.none )

        ChangeAmbientColourR value ->
            let
                ( numeric, textual ) =
                    updateAndParseX model.ambientColour model.ambientColourText value
            in
            ( { model | ambientColour = numeric, ambientColourText = textual }, Cmd.none )

        ChangeAmbientColourG value ->
            let
                ( numeric, textual ) =
                    updateAndParseY model.ambientColour model.ambientColourText value
            in
            ( { model | ambientColour = numeric, ambientColourText = textual }, Cmd.none )

        ChangeAmbientColourB value ->
            let
                ( numeric, textual ) =
                    updateAndParseZ model.ambientColour model.ambientColourText value
            in
            ( { model | ambientColour = numeric, ambientColourText = textual }, Cmd.none )


updateAndParseX : Vec3 -> Triplet -> String -> ( Vec3, Triplet )
updateAndParseX default textual value =
    let
        text =
            { textual | x = value }
    in
    ( parse default text, text )


updateAndParseY : Vec3 -> Triplet -> String -> ( Vec3, Triplet )
updateAndParseY default textual value =
    let
        text =
            { textual | y = value }
    in
    ( parse default text, text )


updateAndParseZ : Vec3 -> Triplet -> String -> ( Vec3, Triplet )
updateAndParseZ default textual value =
    let
        text =
            { textual | z = value }
    in
    ( parse default text, text )


parse : Vec3 -> Triplet -> Vec3
parse default text =
    Maybe.map3 vec3
        (String.toFloat text.x)
        (String.toFloat text.y)
        (String.toFloat text.z)
        |> Maybe.withDefault default


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
        [ onMouseDown (mousePosition MouseDown)
        , onMouseUp (mousePosition MouseUp)
        , onMouseMove (mousePosition MouseMove)
        ]


mouseDown : { x : Int, y : Int } -> Model -> Model
mouseDown { x, y } m =
    { m | mouseStatus = { x = x, y = y, pressed = True } }


mouseMove : { x : Int, y : Int } -> Model -> Model
mouseMove { x, y } m =
    case m.mouseStatus.pressed of
        False ->
            m

        True ->
            { m
                | mouseStatus = { x = x, y = y, pressed = True }
                , thetaX = m.thetaX + degrees (Basics.toFloat (x - m.mouseStatus.x))
                , thetaY = m.thetaY + degrees (Basics.toFloat (y - m.mouseStatus.y))
            }


mouseUp : { x : Int, y : Int } -> Model -> Model
mouseUp { x, y } m =
    { m | mouseStatus = { pressed = False, x = 0, y = 0 } }


mousePosition : ({ x : Int, y : Int } -> Msg) -> Decoder Msg
mousePosition msg =
    Decode.map2 (\x y -> msg { x = x, y = y })
        (Decode.field "pageX" Decode.int)
        (Decode.field "pageY" Decode.int)



-- MESHES


numSegments : Float
numSegments =
    30


sphere : Mesh { position : Vec3, coord : Vec3, norm : Vec3 }
sphere =
    let
        latitudes =
            List.map
                (\idx -> ( Basics.toFloat idx / numSegments, (Basics.toFloat idx + 1) / numSegments ))
                (List.range (-(round numSegments) // 2) ((round numSegments // 2) - 1))
    in
    WebGL.triangles <|
        List.concatMap (\( lat1, lat2 ) -> ring lat1 lat2 numSegments 1) latitudes


ring : Float -> Float -> Float -> Float -> List ( { position : Vec3, coord : Vec3, norm : Vec3 }, { position : Vec3, coord : Vec3, norm : Vec3 }, { position : Vec3, coord : Vec3, norm : Vec3 } )
ring latitude1 latitude2 segments radius =
    let
        longitudes =
            List.map
                (\idx -> ( Basics.toFloat idx / segments, (Basics.toFloat idx + 1) / segments ))
                (List.range 0 (round segments - 1))
    in
    List.concatMap (\( longitude1, longitude2 ) -> face latitude1 latitude2 longitude1 longitude2 radius) longitudes


face : Float -> Float -> Float -> Float -> Float -> List ( { position : Vec3, coord : Vec3, norm : Vec3 }, { position : Vec3, coord : Vec3, norm : Vec3 }, { position : Vec3, coord : Vec3, norm : Vec3 } )
face latitude1 latitude2 longitude1 longitude2 radius =
    let
        theta1 =
            degrees (180 * latitude1)

        theta2 =
            degrees (180 * latitude2)

        phi1 =
            degrees (360 * longitude1)

        phi2 =
            degrees (360 * longitude2)

        topLeft =
            { position = vec3 (cos theta2 * sin phi1 * radius) (sin theta2 * radius) (cos theta2 * cos phi1 * radius), coord = vec3 (longitude1 - 0.5) (latitude2 - 0.5) 0, norm = vec3 (cos theta2 * sin phi1) (sin theta2) (cos theta2 * cos phi1) }

        topRight =
            { position = vec3 (cos theta2 * sin phi2 * radius) (sin theta2 * radius) (cos theta2 * cos phi2 * radius), coord = vec3 (longitude2 - 0.5) (latitude2 - 0.5) 0, norm = vec3 (cos theta2 * sin phi2) (sin theta2) (cos theta2 * cos phi2) }

        bottomLeft =
            { position = vec3 (cos theta1 * sin phi1 * radius) (sin theta1 * radius) (cos theta1 * cos phi1 * radius), coord = vec3 (longitude1 - 0.5) (latitude1 - 0.5) 0, norm = vec3 (cos theta1 * sin phi1) (sin theta1) (cos theta1 * cos phi1) }

        bottomRight =
            { position = vec3 (cos theta1 * sin phi2 * radius) (sin theta1 * radius) (cos theta1 * cos phi2 * radius), coord = vec3 (longitude2 - 0.5) (latitude1 - 0.5) 0, norm = vec3 (cos theta1 * sin phi2) (sin theta1) (cos theta1 * cos phi2) }
    in
    [ ( topLeft, topRight, bottomLeft )
    , ( bottomLeft, topRight, bottomRight )
    ]



-- VIEW


view : Model -> Html Msg
view { texture, thetaX, thetaY, position, lighting, directionalColour, directional, ambientColour, directionalColourText, ambientColourText, directionalText } =
    let
        entities =
            renderEntity sphere thetaX thetaY texture position lighting directionalColour directional ambientColour
    in
    div
        []
        [ WebGL.toHtml
            [ width 400, height 400, style "background" "black" ]
            entities
        , div
            [ style "left" "20px"
            , style "right" "20px"
            , style "top" "500px"
            ]
            [ div []
                [ input [ type_ "checkbox", onClick UseLighting, checked lighting ] []
                , text " Use lighting"
                ]
            , div []
                [ h2 [] [ text "Directional Light" ]
                , div []
                    [ text "Direction: "
                    , text " x: "
                    , input [ type_ "text", step "0.01", onInput ChangeDirectionalX, value directionalText.x ] []
                    , text " y: "
                    , input [ type_ "text", step "0.01", onInput ChangeDirectionalY, value directionalText.y ] []
                    , text " z: "
                    , input [ type_ "text", step "0.01", onInput ChangeDirectionalZ, value directionalText.z ] []
                    ]
                , div []
                    [ text "Colour: "
                    , text " R: "
                    , input [ type_ "text", step "0.01", onInput ChangeDirectionalColourR, value directionalColourText.x ] []
                    , text " G: "
                    , input [ type_ "text", step "0.01", onInput ChangeDirectionalColourG, value directionalColourText.y ] []
                    , text " B: "
                    , input [ type_ "text", step "0.01", onInput ChangeDirectionalColourB, value directionalColourText.z ] []
                    ]
                , h2 [] [ text "Ambient Light" ]
                , div []
                    [ text "Colour: "
                    , text " R: "
                    , input [ type_ "text", step "0.01", onInput ChangeAmbientColourR, value ambientColourText.x ] []
                    , text " G: "
                    , input [ type_ "text", step "0.01", onInput ChangeAmbientColourG, value ambientColourText.y ] []
                    , text " B: "
                    , input [ type_ "text", step "0.01", onInput ChangeAmbientColourB, value ambientColourText.z ] []
                    ]
                ]
            , text message
            ]
        ]


message : String
message =
    "Move the Moon dragging the mouse"


renderEntity : Mesh { position : Vec3, coord : Vec3, norm : Vec3 } -> Float -> Float -> Maybe Texture -> Vec3 -> Bool -> Vec3 -> Vec3 -> Vec3 -> List Entity
renderEntity mesh thetaX thetaY texture position lighting directionalColour directional ambientColour =
    case texture of
        Nothing ->
            []

        Just tex ->
            [ WebGL.entity vertexShader fragmentShader mesh (uniformsShpere thetaX thetaY tex position lighting directionalColour directional ambientColour) ]


uniformsShpere : Float -> Float -> Texture -> Vec3 -> Bool -> Vec3 -> Vec3 -> Vec3 -> { texture : Texture, worldSpace : Mat4, perspective : Mat4, camera : Mat4, normalMatrix : Mat4, lighting : Bool, directionalColour : Vec3, ambientColour : Vec3, directional : Vec3 }
uniformsShpere tx ty texture displacement lighting directionalColour directional ambientColour =
    let
        worldSpace =
            rotate tx (vec3 0 1 0) (rotate ty (vec3 1 0 0) (makeTranslate displacement))

        camera =
            makeLookAt (vec3 0 0 0) (vec3 0 0 -4) (vec3 0 1 0)

        perspective =
            makePerspective 45 1 0.1 100
    in
    { texture = texture
    , worldSpace = worldSpace
    , perspective = perspective
    , camera = camera
    , normalMatrix = transpose (inverseOrthonormal (mul worldSpace camera))
    , lighting = lighting
    , directionalColour = directionalColour
    , ambientColour = ambientColour
    , directional = directional
    }



-- SHADERS


vertexShader : Shader { attr | position : Vec3, coord : Vec3, norm : Vec3 } { unif | worldSpace : Mat4, perspective : Mat4, camera : Mat4, normalMatrix : Mat4, lighting : Bool, directionalColour : Vec3, ambientColour : Vec3, directional : Vec3 } { vcoord : Vec2, lightWeighting : Vec3 }
vertexShader =
    [glsl|

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


fragmentShader : Shader {} { unif | texture : Texture } { vcoord : Vec2, lightWeighting : Vec3 }
fragmentShader =
    [glsl|
  precision mediump float;

  uniform sampler2D texture;
  varying vec2 vcoord;
  varying vec3 lightWeighting;

  void main () {
      vec4 textureColor = texture2D(texture, vec2(vcoord.s, vcoord.t));
      gl_FragColor = vec4(textureColor.rgb * lightWeighting, textureColor.a);
  }

|]
