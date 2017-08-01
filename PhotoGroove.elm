port module PhotoGroove exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick, on)
import Array exposing (Array)
import Random
import Http
import Html.Attributes as Attr exposing (id, class, classList, src, name, type_, title, checked, max)
import Json.Decode exposing (string, int, list, Decoder, at, Value)
import Json.Decode.Pipeline exposing (decode, required, optional)

baseUrl : String
baseUrl = "https://elm-in-action.com/"

port setFilters : FilterOptions -> Cmd msg

port statusChanges : (String -> msg) -> Sub msg

type alias FilterOptions =
  { url : String
  , filters : List { name : String, amount : Float }
  }

type alias Photo =
  { url : String
  , size : Int
  , title : String
  }

type ThumbnailSize
  = Small
  | Medium
  | Large

photoDecoder : Decoder Photo
photoDecoder =
  decode Photo
    |> required "url" string
    |> required "size" int
    |> optional "title" string "(untitled)"

type Msg
  = SelectByUrl String
  | SelectByIndex Int
  | SetStatus String
  | SurpriseMe
  | SetSize ThumbnailSize
  | LoadPhotos (Result Http.Error (List Photo))
  | SetHue Int
  | SetRipple Int
  | SetNoise Int

type alias Model =
  { photos : List Photo
  , status : String
  , selectedUrl : Maybe String
  , loadingError : Maybe String
  , chosenSize : ThumbnailSize
  , hue : Int
  , ripple : Int
  , noise : Int
  }

initialModel : Model
initialModel = 
  {
    photos = []
    , status = ""
    , selectedUrl = Nothing
    , loadingError = Nothing
    , chosenSize = Medium
    , hue = 0
    , ripple = 0
    , noise = 0
  }

view : Model -> Html Msg
view model = 
  div [ class "content"]
    [ h1 [] [ text "Kex's Photo Groove"]
    , button
      [ onClick SurpriseMe ]
      [ text "Surprise Me!" ]
    , div [ class "status" ] [ text model.status ]
    , div [ class "filters" ]
      [ viewFilter "Hue" SetHue model.hue
      , viewFilter "Ripple" SetRipple model.ripple
      , viewFilter "Noise" SetNoise model.noise
      ]
    , h3 [] [ text "Thumbnail Size:"]
    , div [ id "choose-size" ]
      (List.map (viewSizeChooser model.chosenSize) [Small, Medium, Large] )
    , div [ id "thumbnails"
          , class (sizeToString model.chosenSize)
          ]
        (List.map (viewThumbnail model.selectedUrl) model.photos)
    , viewLarge model.selectedUrl
    ]

viewLarge : Maybe String -> Html Msg
viewLarge maybeUrl = 
  case maybeUrl of
    Nothing ->
      text ""
    Just url ->
      canvas [ id "main-canvas"
      , class "large"
      ]
      []

viewThumbnail : Maybe String -> Photo -> Html Msg
viewThumbnail selectedUrl thumbnail = 
    img
      [ src (baseUrl ++ thumbnail.url)
      , title (thumbnail.title ++ " [" ++ toString thumbnail.size ++ " KB]")
      , classList [ ( "selected", selectedUrl == Just thumbnail.url ) ]
      , onClick (SelectByUrl thumbnail.url)
      ]
      []

viewSizeChooser : ThumbnailSize -> ThumbnailSize -> Html Msg
viewSizeChooser defaultSize size = 
  label []
    [ input 
      [ type_ "radio"
      , name "size"
      , checked ( size == defaultSize )
      , onClick (SetSize size)
      ] []
    , text (sizeToString size)
    ]

sizeToString : ThumbnailSize -> String
sizeToString size =
  case size of
    Small ->
      "small"
    Medium ->
      "medium"
    Large ->
      "large"

-- photoArray : Array Photo
-- photoArray =
--   Array.fromList initialModel.photos

-- getPhotoUrl : Int -> Maybe String
-- getPhotoUrl index =
--   case Array.get index photoArray of
--     Just photo ->
--       Just photo.url
--     Nothing ->
--       Nothing

-- randomPhotoPicker : Random.Generator Int
-- randomPhotoPicker =
--   Random.int 0 (Array.length photoArray - 1)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    SelectByUrl selectedUrl ->
      applyFilters { model | selectedUrl = Just selectedUrl }
    SelectByIndex index ->
      let
        newSelectedUrl : Maybe String
        newSelectedUrl =
          model.photos
          |> Array.fromList
          |> Array.get index
          |> Maybe.map .url
      in
        applyFilters { model | selectedUrl = newSelectedUrl }
    SetStatus status ->
      ( { model | status = status}, Cmd.none )
    SurpriseMe ->
      let
        randomPhotoPicker =
          Random.int 0 (List.length model.photos - 1)
      in
        ( model, Random.generate SelectByIndex randomPhotoPicker )
    SetSize size ->
      ( { model | chosenSize = size }, Cmd.none )
    LoadPhotos (Ok photos) ->
      applyFilters
        { model
          | photos = photos
          , selectedUrl = Maybe.map .url (List.head photos)
        }
    LoadPhotos (Err _) ->
      ( { model
          | loadingError = Just "Error! (Try turning off and on again?)"
        }
      , Cmd.none
      )
    SetHue hue ->
      applyFilters { model | hue = hue }
    SetRipple ripple ->
      applyFilters { model | ripple = ripple }
    SetNoise noise ->
      applyFilters { model | noise = noise }

applyFilters : Model -> ( Model, Cmd Msg )
applyFilters model =
  case model.selectedUrl of
    Just selectedUrl ->
      let
        filters =
          [ { name = "Hue",  amount = toFloat model.hue / 11}
          , { name = "Ripple", amount = toFloat model.ripple / 11 }
          , { name = "Noise", amount = toFloat model.noise / 11 }
          ]
        url =
          baseUrl ++ "large/" ++ selectedUrl
        cmd =
          setFilters { url = url, filters = filters }
      in
        ( model , cmd )
    Nothing ->
      ( model, Cmd.none )

viewOnError : Model -> Html Msg
viewOnError model =
  case model.loadingError of
    Nothing ->
      view model
    Just errorMessage ->
      div [ class "error-message" ]
          [ h1 [] [ text "Photo Groove" ]
          , p [] [ text errorMessage ]
          ]

viewFilter : String -> (Int -> Msg) -> Int -> Html Msg
viewFilter name toMsg magnitude =
  div [ class "filter-slider" ]
      [ label [] [ text name ]
      , paperSlider 
        [ Attr.max "11"
        , onImmediateValueChange toMsg
        ]
        []
      , label [] [ text (toString magnitude) ]
      ]

paperSlider : List (Attribute msg) -> List (Html msg) -> Html msg
paperSlider =
  node "paper-slider"

onImmediateValueChange : (Int -> msg) -> Attribute msg
onImmediateValueChange toMsg =
      at [ "target", "immediateValue" ] int
        |> Json.Decode.map toMsg
        |> on "immediate-value-changed"

initialCmd: Cmd Msg
initialCmd =
  list photoDecoder
    |> Http.get (baseUrl ++ "photos/list.json")
    |> Http.send LoadPhotos

main : Program Value Model Msg
main =
  Html.programWithFlags
  { init = init
  , view = viewOnError
  , update = update
  , subscriptions = \_ -> statusChanges SetStatus
  }

init : Value -> ( Model, Cmd Msg )
init flags =
  let
    status =
      "Initializing Pasta v" ++ toString flags
  in
    ( { initialModel | status = status }, initialCmd )