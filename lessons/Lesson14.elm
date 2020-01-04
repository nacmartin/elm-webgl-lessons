module Lesson14 exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta, onKeyDown, onKeyUp)
import Html exposing (Html, div, h2, input, text)
import Html.Attributes exposing (checked, height, step, style, type_, value, width)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode exposing (..)
import List.Extra exposing (getAt, greedyGroupsOf)
import Math.Matrix4 exposing (..)
import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (..)
import String
import Task exposing (Task, succeed)
import WebGL exposing (Entity, Mesh, Shader)
import WebGL.Texture as Texture exposing (Error, Texture, defaultOptions)


type alias Vertex =
    { position : Vec3, coord : Vec3, normal : Vec3 }


type alias Triplet =
    { x : String
    , y : String
    , z : String
    }


type alias Model =
    { texture : Maybe Texture
    , position : Vec3
    , world : Mesh Vertex
    , useLighting : Bool
    , useSpecular : Bool
    , useTextures : Bool
    , shininess : Float
    , shininessText : String
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
    = TextureLoaded (Result Error Texture)
    | Animate Float
    | WorldLoaded (Result Http.Error (Mesh Vertex))
    | UseLighting
    | UseSpecular
    | UseTextures
    | ChangeShininess String
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
    ( { texture = Nothing
      , position = vec3 0 0 0
      , world = WebGL.triangles []
      , useLighting = True
      , useTextures = True
      , useSpecular = True
      , specularColor = vec3 1 1 1
      , specularColorText = { x = "1", y = "1", z = "1" }
      , ambientColor = vec3 0.2 0.2 0.2
      , ambientColorText = { x = "0.2", y = "0.2", z = "0.2" }
      , diffuseColor = vec3 0.8 0.8 0.8
      , diffuseColorText = { x = "0.8", y = "0.8", z = "0.8" }
      , specular = vec3 -10 4 20
      , specularText = { x = "-10", y = "4", z = "20" }
      , theta = 0
      , shininessText = "32.0"
      , shininess = 32.0
      }
    , Cmd.batch
        [ Texture.loadWith { defaultOptions | minify = Texture.linear, magnify = Texture.linear } "textures/metal.jpg"
            |> Task.attempt TextureLoaded
        , Http.get
            { url = "meshes/Teapot.json"
            , expect =
                Http.expectJson
                    (Result.map getWorld >> WorldLoaded)
                    worldDecoder
            }
        ]
    )


type alias World =
    { vertexPositions : List Float, vertexNormals : List Float, vertexTextureCoords : List Float, indices : List Int }


makeVec3 : List Float -> Vec3
makeVec3 coords =
    case coords of
        a :: b :: [] ->
            vec3 a b 0

        a :: b :: c :: [] ->
            vec3 a b c

        _ ->
            vec3 0 0 0


getWorld { vertexPositions, vertexNormals, vertexTextureCoords, indices } =
    let
        vertexes =
            List.map3
                (\a b c ->
                    { position = makeVec3 a
                    , coord = makeVec3 b
                    , normal = makeVec3 c
                    }
                )
                (greedyGroupsOf 3 vertexPositions)
                (greedyGroupsOf 2 vertexTextureCoords)
                (greedyGroupsOf 3 vertexNormals)

        groupIndicesBy3 currentIndices result =
            case currentIndices of
                i1 :: i2 :: i3 :: restIndices ->
                    groupIndicesBy3 restIndices (( i1, i2, i3 ) :: result)

                _ ->
                    List.reverse result
    in
    WebGL.indexedTriangles vertexes (groupIndicesBy3 indices [])


worldDecoder : Decoder World
worldDecoder =
    Decode.map4 World
        (Decode.field "vertexPositions" (Decode.list Decode.float))
        (Decode.field "vertexNormals" (Decode.list Decode.float))
        (Decode.field "vertexTextureCoords" (Decode.list Decode.float))
        (Decode.field "indices" (Decode.list Decode.int))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        WorldLoaded world ->
            ( { model | world = Result.withDefault model.world world }, Cmd.none )

        TextureLoaded texture ->
            ( { model | texture = Result.toMaybe texture }, Cmd.none )

        Animate dt ->
            ( { model | theta = model.theta + dt / 1000 }
            , Cmd.none
            )

        UseLighting ->
            ( { model | useLighting = not model.useLighting }, Cmd.none )

        UseTextures ->
            ( { model | useTextures = not model.useTextures }, Cmd.none )

        UseSpecular ->
            ( { model | useSpecular = not model.useSpecular }, Cmd.none )

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

        ChangeShininess value ->
            let
                parsed =
                    case String.toFloat value of
                        Just val ->
                            val

                        _ ->
                            model.shininess
            in
            ( { model | shininess = parsed, shininessText = value }, Cmd.none )


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
        , subscriptions = \_ -> onAnimationFrameDelta Animate
        , update = update
        }



-- VIEW


view : Model -> Html Msg
view { texture, theta, shininess, world, position, useLighting, useTextures, useSpecular, specular, specularText, specularColor, specularColorText, ambientColor, shininessText, ambientColorText, diffuseColorText, diffuseColor } =
    div
        []
        [ WebGL.toHtml
            [ width 600, height 600, style "background" "black" ]
            (renderEntity world theta shininess texture position useLighting useSpecular useTextures specularColor specular ambientColor diffuseColor)
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
                [ input [ type_ "checkbox", onClick UseSpecular, checked useSpecular ] []
                , text " Use specular lighting"
                ]
            , div []
                [ input [ type_ "checkbox", onClick UseTextures, checked useTextures ] []
                , text " Use textures"
                ]
            , div []
                [ h2 [] [ text "Material" ]
                , div []
                    [ text "Shininess: "
                    , input [ type_ "text", onInput ChangeShininess, Html.Attributes.value shininessText ] []
                    ]
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


renderEntity : Mesh { position : Vec3, coord : Vec3, normal : Vec3 } -> Float -> Float -> Maybe Texture -> Vec3 -> Bool -> Bool -> Bool -> Vec3 -> Vec3 -> Vec3 -> Vec3 -> List Entity
renderEntity world theta shininess texture position useLighting useSpecular useTextures specularColor specular ambientColor diffuseColor =
    case texture of
        Nothing ->
            []

        Just tex ->
            [ WebGL.entity vertexShader fragmentShader world (uniforms tex theta shininess position useLighting useSpecular useTextures specularColor specular ambientColor diffuseColor) ]


uniforms : Texture -> Float -> Float -> Vec3 -> Bool -> Bool -> Bool -> Vec3 -> Vec3 -> Vec3 -> Vec3 -> { texture : Texture, perspective : Mat4, camera : Mat4, worldSpace : Mat4, useLighting : Bool, normalMatrix : Mat4, useSpecular : Bool, useTextures : Bool, specularColor : Vec3, ambientColor : Vec3, diffuseColor : Vec3, specular : Vec3, shininess : Float }
uniforms texture theta shininess position useLighting useSpecular useTextures specularColor specular ambientColor diffuseColor =
    let
        worldSpace =
            translate position (rotate (degrees 23.4) (vec3 1 0 1) (makeRotate theta (vec3 0 1 0)))

        camera =
            makeLookAt (vec3 0 0 50) (vec3 0 0 -1) (vec3 0 1 0)

        perspective =
            makePerspective 45 1 0.1 100
    in
    { texture = texture
    , worldSpace = worldSpace
    , camera = camera
    , perspective = perspective
    , normalMatrix = transpose (inverseOrthonormal worldSpace)
    , useLighting = useLighting
    , useSpecular = useSpecular
    , useTextures = useTextures
    , specularColor = specularColor
    , ambientColor = ambientColor
    , diffuseColor = diffuseColor
    , specular = specular
    , shininess = shininess
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


fragmentShader : Shader {} { unif | texture : Texture, useSpecular : Bool, useLighting : Bool, useTextures : Bool, ambientColor : Vec3, specularColor : Vec3, diffuseColor : Vec3, specular : Vec3, shininess : Float } { vcoord : Vec2, vPosition : Vec3, vTransformedNormal : Vec3 }
fragmentShader =
    [glsl|
  precision mediump float;

  uniform sampler2D texture;
  uniform bool useLighting;
  uniform bool useSpecular;
  uniform bool useTextures;
  uniform vec3 ambientColor;
  uniform vec3 diffuseColor;
  uniform vec3 specularColor;
  uniform vec3 specular;
  uniform float shininess;

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
        if (useSpecular) {
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
      if (useTextures) {
        fragmentColor = texture2D(texture, vec2(vcoord.s, vcoord.t));
      } else {
        fragmentColor = vec4(1.0, 1.0, 1.0, 1.0);
      }
      gl_FragColor = vec4(fragmentColor.rgb * lightWeighting, fragmentColor.a);
  }

|]
