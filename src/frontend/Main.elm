module Main exposing (..)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Http
import Json.Decode exposing (Decoder, field, string, list)



-- MAIN


main =
  Browser.document
    { init = init
    , subscriptions = subscriptions
    , update = update
    , view = view
    }



-- MODEL


type Model
  = Failure
  | Loading
  | JailsSuccess Jails
  | JailSuccess Jail


type alias Jails = List String
type alias Jail =
  { currentlyBanned : Int, totalBanned : Int, bannedIPs : [String]
  , ignoredIPs : [String], findTime : Int, banTime : Int, maxRetry : Int }


init : () -> (Model, Cmd Msg)
init _ =
        (Loading
        , getJails)



-- UPDATE


type Msg
  = GotJails (Result Http.Error Jails)
  | GotJail (Result Http.Error Jail)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotJails result ->
      case result of
        Ok jails -> (JailsSuccess jails, Cmd.none)
        Err _ -> (Failure, Cmd.none)
      

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- VIEW


view : Model -> Browser.Document Msg
view model =
  { title = "fail2panel"
  , body = [
    layout [ width fill, height fill ] <|
      row [ width <| minimum 300 fill, height fill ]
    (case model of
      Failure ->
        [text "Jails failed to load."]

      Loading ->
        [text "Loading..."]

      JailsSuccess jails ->
        [ viewJails jails ""
        , text "Bottom text"
        ]
      )
    ]
  }

viewJails : Jails -> String -> Element msg
viewJails jails activeJail =
  column
    [ Background.gradient {angle = 0, steps = [rgb255 0 194 65, rgb255 20 163 247]}
    , height fill
    , width (px 300)
    , spacing 5
    ] (el
        [ Font.color (rgb 1 1 1)
        , Font.size 26
        , Font.bold
        , width fill
        , paddingXY 25 20
        ]
        (text "FAIL2PANEL") ::
    (List.append (List.map (\jail -> viewJail jail activeJail) jails) (List.map (\jail -> viewJail jail activeJail) jails)))

viewJail : String -> String -> Element msg
viewJail jail activeJail =
  let activeJailAttrs =
        [ Background.gradient {angle = 0.5 * pi, steps = [rgb255 60 231 117, rgb255 64 183 254]}]
      normalAttrs = 
        [ Font.color (rgb 1 1 1)
        , Font.size 18
        , width fill
        , paddingXY 25 15
        , mouseOver [ Background.color (rgba 1 1 1 0.1)]
        ]
      attrs = (if (jail == activeJail) then
            normalAttrs ++ activeJailAttrs
          else
            normalAttrs
          )
  in
  el
    attrs
    (text jail)


-- HTTP
getJails : Cmd Msg
getJails =
  Http.get
    { url = "http://localhost:5000/api/jails"
    , expect = Http.expectJson GotJails jailsDecoder
    }


jailsDecoder : Decoder Jails
jailsDecoder =
  field "jails" (list string)


getJail : String -> Cmd Msg
getJail jail =
  Http.get
    { url = "http://localhost:5000/api/jails/" ++ jail
    , expect = Http.expectJson GotJail jailDecoder
    }


jailDecoder : Decoder Jail
jailDecoder =
  field "jails" (list string)
