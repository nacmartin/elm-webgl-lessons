module Lesson15 exposing (main)

import Array
import Browser
import Browser.Events exposing (onAnimationFrameDelta, onKeyDown, onKeyUp)
import Html exposing (Html, div, h2, input, text)
import Html.Attributes exposing (checked, height, step, style, type_, value, width)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode exposing (..)
import List.Extra exposing (getAt, greedyGroupsOf)
import Math.Matrix4 exposing (..)
import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (..)
import Random
import Regex
import String
import Task exposing (Task, succeed)
import WebGL exposing (Entity, Mesh, Shader)
import WebGL.Texture as Texture exposing (Error, Texture, defaultOptions)


type alias Triplet =
    { x : String
    , y : String
    , z : String
    }


type alias Model =
    { textures : Maybe ( Texture, Texture )
    , position : Vec3
    , useLighting : Bool
    , useSpecularMap : Bool
    , useColorMap : Bool
    , ambientColorText : { x : String, y : String, z : String }
    , ambientColor : Vec3
    , diffuseColorText : { x : String, y : String, z : String }
    , diffuseColor : Vec3
    , specularText : { x : String, y : String, z : String }
    , specular : Vec3
    , specularColorText : { x : String, y : String, z : String }
    , specularColor : Vec3
    , theta : Float
    }


type Msg
    = TexturesLoaded (Result Error ( Texture, Texture ))
    | Animate Float
    | UseLighting
    | UseSpecularMap
    | UseColorMap
    | ChangeSpecularColorR String
    | ChangeSpecularColorG String
    | ChangeSpecularColorB String
    | ChangeAmbientColorR String
    | ChangeAmbientColorG String
    | ChangeAmbientColorB String
    | ChangeDiffuseColorR String
    | ChangeDiffuseColorG String
    | ChangeDiffuseColorB String
    | ChangeSpecularX String
    | ChangeSpecularY String
    | ChangeSpecularZ String


init : ( Model, Cmd Msg )
init =
    ( { textures = Nothing
      , position = vec3 0 0 0
      , useLighting = True
      , useColorMap = True
      , useSpecularMap = True
      , specularColor = vec3 5 5 5
      , specularColorText = { x = "5", y = "5", z = "5" }
      , ambientColor = vec3 0.2 0.2 0.2
      , ambientColorText = { x = "0.2", y = "0.2", z = "0.2" }
      , diffuseColor = vec3 0.8 0.8 0.8
      , diffuseColorText = { x = "0.8", y = "0.8", z = "0.8" }
      , specular = vec3 -10 4 20
      , specularText = { x = "-10", y = "4", z = "20" }
      , theta = 0
      }
    , Task.map2 Tuple.pair
        (Texture.loadWith { defaultOptions | minify = Texture.linear, magnify = Texture.linear } "textures/earth.jpg")
        (Texture.loadWith { defaultOptions | minify = Texture.linear, magnify = Texture.linear } "textures/earth-specular.gif")
        |> Task.attempt TexturesLoaded
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TexturesLoaded textures ->
            ( { model | textures = Result.toMaybe textures }, Cmd.none )

        Animate dt ->
            ( { model | theta = model.theta + dt / 1000 }
            , Cmd.none
            )

        UseLighting ->
            ( { model | useLighting = not model.useLighting }, Cmd.none )

        UseColorMap ->
            ( { model | useColorMap = not model.useColorMap }, Cmd.none )

        UseSpecularMap ->
            ( { model | useSpecularMap = not model.useSpecularMap }, Cmd.none )

        ChangeSpecularX value ->
            let
                ( numeric, textual ) =
                    updateAndParseX model.specular model.specularText value
            in
            ( { model | specular = numeric, specularText = textual }, Cmd.none )

        ChangeSpecularY value ->
            let
                ( numeric, textual ) =
                    updateAndParseY model.specular model.specularText value
            in
            ( { model | specular = numeric, specularText = textual }, Cmd.none )

        ChangeSpecularZ value ->
            let
                ( numeric, textual ) =
                    updateAndParseZ model.specular model.specularText value
            in
            ( { model | specular = numeric, specularText = textual }, Cmd.none )

        ChangeSpecularColorR value ->
            let
                ( numeric, textual ) =
                    updateAndParseX model.specularColor model.specularColorText value
            in
            ( { model | specularColor = numeric, specularColorText = textual }, Cmd.none )

        ChangeSpecularColorG value ->
            let
                ( numeric, textual ) =
                    updateAndParseY model.specularColor model.specularColorText value
            in
            ( { model | specularColor = numeric, specularColorText = textual }, Cmd.none )

        ChangeSpecularColorB value ->
            let
                ( numeric, textual ) =
                    updateAndParseZ model.specularColor model.specularColorText value
            in
            ( { model | specularColor = numeric, specularColorText = textual }, Cmd.none )

        ChangeAmbientColorR value ->
            let
                ( numeric, textual ) =
                    updateAndParseX model.ambientColor model.ambientColorText value
            in
            ( { model | ambientColor = numeric, ambientColorText = textual }, Cmd.none )

        ChangeAmbientColorG value ->
            let
                ( numeric, textual ) =
                    updateAndParseY model.ambientColor model.ambientColorText value
            in
            ( { model | ambientColor = numeric, ambientColorText = textual }, Cmd.none )

        ChangeAmbientColorB value ->
            let
                ( numeric, textual ) =
                    updateAndParseZ model.ambientColor model.ambientColorText value
            in
            ( { model | ambientColor = numeric, ambientColorText = textual }, Cmd.none )

        ChangeDiffuseColorR value ->
            let
                ( numeric, textual ) =
                    updateAndParseX model.diffuseColor model.diffuseColorText value
            in
            ( { model | diffuseColor = numeric, diffuseColorText = textual }, Cmd.none )

        ChangeDiffuseColorG value ->
            let
                ( numeric, textual ) =
                    updateAndParseY model.diffuseColor model.diffuseColorText value
            in
            ( { model | diffuseColor = numeric, diffuseColorText = textual }, Cmd.none )

        ChangeDiffuseColorB value ->
            let
                ( numeric, textual ) =
                    updateAndParseZ model.diffuseColor model.diffuseColorText value
            in
            ( { model | diffuseColor = numeric, diffuseColorText = textual }, Cmd.none )


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
    onAnimationFrameDelta Animate



-- MESHES


numSegments : Float
numSegments =
    40


sphere : Mesh { position : Vec3, coord : Vec3, normal : Vec3 }
sphere =
    let
        latitudes =
            List.map (\idx -> ( Basics.toFloat idx / numSegments, (Basics.toFloat idx + 1) / numSegments )) (List.range (-(round numSegments) // 2) ((round numSegments // 2) - 1))
    in
    WebGL.triangles <|
        List.concatMap (\( lat1, lat2 ) -> ring lat1 lat2 numSegments 1) latitudes


ring : Float -> Float -> Float -> Float -> List ( { position : Vec3, coord : Vec3, normal : Vec3 }, { position : Vec3, coord : Vec3, normal : Vec3 }, { position : Vec3, coord : Vec3, normal : Vec3 } )
ring latitude1 latitude2 segments radius =
    let
        longitudes =
            List.map (\idx -> ( Basics.toFloat idx / segments, (Basics.toFloat idx + 1) / segments )) (List.range 0 (round segments - 1))
    in
    List.concatMap (\( longitude1, longitude2 ) -> sphereFace latitude1 latitude2 longitude1 longitude2 radius) longitudes


sphereFace : Float -> Float -> Float -> Float -> Float -> List ( { position : Vec3, coord : Vec3, normal : Vec3 }, { position : Vec3, coord : Vec3, normal : Vec3 }, { position : Vec3, coord : Vec3, normal : Vec3 } )
sphereFace latitude1 latitude2 longitude1 longitude2 radius =
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
            { position = vec3 (cos theta2 * sin phi1 * radius) (sin theta2 * radius) (cos theta2 * cos phi1 * radius), coord = vec3 (longitude1 - 0.5) (latitude2 - 0.5) 0, normal = vec3 (cos theta2 * sin phi1) (sin theta2) (cos theta2 * cos phi1) }

        topRight =
            { position = vec3 (cos theta2 * sin phi2 * radius) (sin theta2 * radius) (cos theta2 * cos phi2 * radius), coord = vec3 (longitude2 - 0.5) (latitude2 - 0.5) 0, normal = vec3 (cos theta2 * sin phi2) (sin theta2) (cos theta2 * cos phi2) }

        bottomLeft =
            { position = vec3 (cos theta1 * sin phi1 * radius) (sin theta1 * radius) (cos theta1 * cos phi1 * radius), coord = vec3 (longitude1 - 0.5) (latitude1 - 0.5) 0, normal = vec3 (cos theta1 * sin phi1) (sin theta1) (cos theta1 * cos phi1) }

        bottomRight =
            { position = vec3 (cos theta1 * sin phi2 * radius) (sin theta1 * radius) (cos theta1 * cos phi2 * radius), coord = vec3 (longitude2 - 0.5) (latitude1 - 0.5) 0, normal = vec3 (cos theta1 * sin phi2) (sin theta1) (cos theta1 * cos phi2) }
    in
    [ ( topLeft, topRight, bottomLeft )
    , ( bottomLeft, topRight, bottomRight )
    ]



-- VIEW


view : Model -> Html Msg
view { textures, theta, position, useLighting, useColorMap, useSpecularMap, specular, specularText, specularColor, specularColorText, ambientColor, ambientColorText, diffuseColorText, diffuseColor } =
    div
        []
        [ WebGL.toHtml
            [ width 600, height 600, style "background" "black" ]
            (renderEntity theta textures position useLighting useSpecularMap useColorMap specularColor specular ambientColor diffuseColor)
        , div
            [ style "left" "20px"
            , style "right" "20px"
            , style "top" "500px"
            ]
            [ div []
                [ input [ type_ "checkbox", onClick UseLighting, checked useLighting ] []
                , text " Use lighting"
                ]
            , div []
                [ input [ type_ "checkbox", onClick UseSpecularMap, checked useSpecularMap ] []
                , text " Use specular map"
                ]
            , div []
                [ input [ type_ "checkbox", onClick UseColorMap, checked useColorMap ] []
                , text " Use textures"
                ]
            , div []
                [ h2 [] [ text "Point Light" ]
                , div []
                    [ text "Position: "
                    , text " x: "
                    , input [ type_ "text", onInput ChangeSpecularX, Html.Attributes.value specularText.x ] []
                    , text " y: "
                    , input [ type_ "text", onInput ChangeSpecularY, Html.Attributes.value specularText.y ] []
                    , text " z: "
                    , input [ type_ "text", onInput ChangeSpecularZ, Html.Attributes.value specularText.z ] []
                    ]
                , div []
                    [ text "Specular color: "
                    , text " R: "
                    , input [ type_ "text", onInput ChangeSpecularColorR, Html.Attributes.value specularColorText.x ] []
                    , text " G: "
                    , input [ type_ "text", onInput ChangeSpecularColorG, Html.Attributes.value specularColorText.y ] []
                    , text " B: "
                    , input [ type_ "text", onInput ChangeSpecularColorB, Html.Attributes.value specularColorText.z ] []
                    ]
                , div []
                    [ text "Diffuse color: "
                    , text " R: "
                    , input [ type_ "text", onInput ChangeDiffuseColorR, Html.Attributes.value diffuseColorText.x ] []
                    , text " G: "
                    , input [ type_ "text", onInput ChangeDiffuseColorG, Html.Attributes.value diffuseColorText.y ] []
                    , text " B: "
                    , input [ type_ "text", onInput ChangeDiffuseColorB, Html.Attributes.value diffuseColorText.z ] []
                    ]
                , h2 [] [ text "Ambient Light" ]
                , div []
                    [ text "Color: "
                    , text " R: "
                    , input [ type_ "text", onInput ChangeAmbientColorR, Html.Attributes.value ambientColorText.x ] []
                    , text " G: "
                    , input [ type_ "text", onInput ChangeAmbientColorG, Html.Attributes.value ambientColorText.y ] []
                    , text " B: "
                    , input [ type_ "text", onInput ChangeAmbientColorB, Html.Attributes.value ambientColorText.z ] []
                    ]
                ]
            ]
        ]


renderEntity : Float -> Maybe ( Texture, Texture ) -> Vec3 -> Bool -> Bool -> Bool -> Vec3 -> Vec3 -> Vec3 -> Vec3 -> List Entity
renderEntity theta textures position useLighting useSpecularMap useColorMap specularColor specular ambientColor diffuseColor =
    case textures of
        Just ( colorMap, specularMap ) ->
            [ WebGL.entity vertexShader fragmentShader sphere (uniforms colorMap specularMap theta position useLighting useSpecularMap useColorMap specularColor specular ambientColor diffuseColor) ]

        Nothing ->
            []


uniforms : Texture -> Texture -> Float -> Vec3 -> Bool -> Bool -> Bool -> Vec3 -> Vec3 -> Vec3 -> Vec3 -> { colorMap : Texture, specularMap : Texture, perspective : Mat4, camera : Mat4, worldSpace : Mat4, useLighting : Bool, normalMatrix : Mat4, useSpecularMap : Bool, useColorMap : Bool, specularColor : Vec3, ambientColor : Vec3, diffuseColor : Vec3, specular : Vec3 }
uniforms colorMap specularMap theta position useLighting useSpecularMap useColorMap specularColor specular ambientColor diffuseColor =
    let
        worldSpace =
            translate position (makeRotate theta (vec3 0 1 0))

        camera =
            makeLookAt (vec3 0 0 3) (vec3 0 0 -1) (vec3 0 1 0)

        perspective =
            makePerspective 45 1 0.1 100
    in
    { colorMap = colorMap
    , specularMap = specularMap
    , worldSpace = worldSpace
    , camera = camera
    , perspective = perspective
    , normalMatrix = transpose (inverseOrthonormal worldSpace)
    , useLighting = useLighting
    , useSpecularMap = useSpecularMap
    , useColorMap = useColorMap
    , specularColor = specularColor
    , ambientColor = ambientColor
    , diffuseColor = diffuseColor
    , specular = specular
    }



-- SHADERS


vertexShader : Shader { attr | position : Vec3, coord : Vec3, normal : Vec3 } { unif | worldSpace : Mat4, perspective : Mat4, camera : Mat4, normalMatrix : Mat4 } { vcoord : Vec2, vPosition : Vec3, vTransformedNormal : Vec3 }
vertexShader =
    [glsl|

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


fragmentShader : Shader {} { unif | colorMap : Texture, specularMap : Texture, useSpecularMap : Bool, useLighting : Bool, useColorMap : Bool, ambientColor : Vec3, specularColor : Vec3, diffuseColor : Vec3, specular : Vec3 } { vcoord : Vec2, vPosition : Vec3, vTransformedNormal : Vec3 }
fragmentShader =
    [glsl|
  precision mediump float;

  uniform sampler2D colorMap;
  uniform sampler2D specularMap;
  uniform bool useLighting;
  uniform bool useSpecularMap;
  uniform bool useColorMap;
  uniform vec3 ambientColor;
  uniform vec3 diffuseColor;
  uniform vec3 specularColor;
  uniform vec3 specular;

  varying vec2 vcoord;
  varying vec3 vPosition;
  varying vec3 vTransformedNormal;

  void main () {
      vec3 lightWeighting;
      lightWeighting = vec3(1.0, 1.0, 1.0);
      if (!useLighting) {
        lightWeighting = vec3(1.0, 1.0, 1.0);
      } else {

        vec3 lightDirection = normalize(specular - vPosition);
        vec3 normal = normalize(vTransformedNormal);

        float specularLightWeighting = 0.0;
        float shininess = 32.0;
        if (useSpecularMap) {
            shininess = texture2D(specularMap, vec2(vcoord.s, vcoord.t)).r * 255.0;
        }
        if (shininess < 255.0) {
            vec3 eyeDirection = normalize(-vPosition.xyz);
            vec3 reflectionDirection = reflect(-lightDirection, normal);

            specularLightWeighting = pow(max(dot(reflectionDirection, eyeDirection), 0.0), shininess);
        }

        float diffuseLightWeighting = max(dot(normal, lightDirection), 0.0);
        lightWeighting = ambientColor
            + specularColor * specularLightWeighting
            + diffuseColor * diffuseLightWeighting;
      }
      vec4 fragmentColor;
      if (useColorMap) {
        fragmentColor = texture2D(colorMap, vec2(vcoord.s, vcoord.t));
      } else {
        fragmentColor = vec4(1.0, 1.0, 1.0, 1.0);
      }
      gl_FragColor = vec4(fragmentColor.rgb * lightWeighting, fragmentColor.a);
  }

|]
