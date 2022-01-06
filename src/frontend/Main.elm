module Main exposing (..)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Http
import Json.Decode exposing (Decoder, map7, field, int, string, list)



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
  | JailsPage JailsPageModel

type alias JailsPageModel =
  { jails : Jails, activeJail : Maybe String, jailInfo : Maybe Jail }

type alias Jails = List String
type alias Jail =
  { currentlyBanned : Int, totalBanned : Int, bannedIPs : List String, ignoredIPs : List String, findTime : Int, banTime : Int, maxRetry : Int }

initialModel = JailsPageModel [] Nothing Nothing


init : () -> (Model, Cmd Msg)
init _ = (JailsPage initialModel, getJails)



-- UPDATE


type Msg
  = GotJails (Result Http.Error Jails)
  | DeselectJails
  | SelectedJail String
  | GotJail String (Result Http.Error Jail)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotJails result ->
      case result of
        Ok jailList ->
          case model of
            Failure -> (JailsPage { initialModel | jails = jailList }, Cmd.none)
            JailsPage jpModel -> (JailsPage { jpModel | jails = jailList }, Cmd.none)
        Err _ -> (Failure, Cmd.none)
    DeselectJails ->
      case model of
        Failure -> init ()
        JailsPage jpModel -> (JailsPage { jpModel | activeJail = Nothing, jailInfo = Nothing }, Cmd.none)
    SelectedJail jailName ->
      case model of
        Failure -> (Failure, Cmd.none)
        JailsPage jpModel -> (JailsPage { jpModel | activeJail = Just jailName }, getJail jailName)
    GotJail jailName result ->
      case result of
        Ok jail -> 
          case model of
            Failure -> init ()
            JailsPage jpModel -> (JailsPage { jpModel | jailInfo = Just jail }, Cmd.none)
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
        [ Element.column [ alignTop, alignLeft, Font.size 30, Font.bold, paddingXY 10 10, width fill, spacing 10 ]
          [ text "Jails failed to load."
          , paragraph [ Font.size 20, Font.regular ] [ text "Maybe fail2ban isn't started, this server's user can't access fail2ban's socket, or the backend daemon isn't running." ]
          ]
        ]

      JailsPage jpModel ->
        [ viewJails jpModel.jails jpModel.activeJail
        , viewJailInfo jpModel.jailInfo
        ]
      )
    ]
  }

viewJails : Jails -> Maybe String -> Element Msg
viewJails jails activeJail =
  column
    [ Background.gradient {angle = 0, steps = [rgb255 0 194 65, rgb255 20 163 247]}
    , height fill
    , width (px 300)
    , spacing 5
    , scrollbarY
    ] (link [] { url = "#", label = el
        [ Font.color (rgb 1 1 1)
        , Font.size 26
        , Font.bold
        , width fill
        , paddingXY 25 20
        , onClick DeselectJails
        ]
        (text "FAIL2PANEL")} ::
    (List.map (\jail -> viewJail jail activeJail) jails))

viewJail : String -> Maybe String -> Element Msg
viewJail jail activeJail =
  let activeJailAttrs =
        [ Background.gradient {angle = 0.5 * pi, steps = [rgb255 60 231 117, rgb255 64 183 254]}]
      normalAttrs = 
        [ Font.color (rgb 1 1 1)
        , Font.size 18
        , width fill
        , paddingXY 25 15
        , mouseOver [ Background.color (rgba 1 1 1 0.1)]
        , onClick (SelectedJail jail)
        ]
      attrs = case activeJail of
        Nothing -> normalAttrs
        Just active ->
          if (jail == active) then
            normalAttrs ++ activeJailAttrs
          else
            normalAttrs
  in
  link attrs { url = "#", label = el [] (text jail) }

viewJailInfo : Maybe Jail -> Element msg
viewJailInfo maybeJail =
  case maybeJail of
    Nothing -> el [] (text "Bottom text lmao")
    Just jailInfo -> el [] (text "You've got jail")


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
    , expect = Http.expectJson (GotJail jail) jailDecoder
    }

jailDecoder : Decoder Jail
jailDecoder = map7 Jail
  (field "currently_banned" int)
  (field "total_banned" int)
  (field "banned_ips" (list string))
  (field "ignored_ips" (list string))
  (field "findtime" int)
  (field "bantime" int)
  (field "maxretry" int)
