module Main exposing (..)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Input as Input
import Http
import Json.Decode as Decode exposing (Decoder, field, int, string, list)
import Json.Decode.Pipeline exposing (required)



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
  { currentlyFailed : Int
  , totalFailed : Int
  , fileList : List String
  , currentlyBanned : Int
  , totalBanned : Int
  , bannedIPs : List String
  , bannedASNs : List String
  , bannedCountries : List String
  , bannedRIRs : List String
  , ignoredIPs : List String
  , findTime : Int
  , banTime : Int
  , maxRetry : Int
  }

type alias IP =
  { addr : String
  , asn : String
  , country : String
  , rir : String
  }

initialModel = JailsPageModel [] Nothing Nothing


init : () -> (Model, Cmd Msg)
init _ = (JailsPage initialModel, getJails)



-- UPDATE


type Msg
  = GotJails (Result Http.Error Jails)
  | DeselectJails
  | SelectedJail String
  | UnbanIP String
  | UnbannedIP (Result Http.Error ())
  | UnignoreIP String
  | UnignoredIP (Result Http.Error ())
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
        Failure -> init () 
        JailsPage jpModel -> (JailsPage { jpModel | activeJail = Just jailName }, getJail jailName)
    UnbanIP ip ->
      case model of
        Failure -> init () 
        JailsPage jpModel ->
          case jpModel.activeJail of
            Nothing -> init ()
            Just activeJail -> (JailsPage jpModel, postUnbanIP activeJail ip)
    UnbannedIP result -> 
      case result of
        Ok _ ->
          case model of
            Failure -> init ()
            JailsPage jpModel ->
              case jpModel.activeJail of
                Nothing -> (Failure, Cmd.none)
                Just activeJail -> (JailsPage jpModel, getJail activeJail)
        Err _ -> (Failure, Cmd.none)
    UnignoreIP ip ->
      case model of
        Failure -> init () 
        JailsPage jpModel ->
          case jpModel.activeJail of
            Nothing -> init ()
            Just activeJail -> (JailsPage jpModel, postUnignoreIP activeJail ip)
    UnignoredIP result -> 
      case result of
        Ok _ ->
          case model of
            Failure -> init ()
            JailsPage jpModel ->
              case jpModel.activeJail of
                Nothing -> (Failure, Cmd.none)
                Just activeJail -> (JailsPage jpModel, getJail activeJail)
        Err _ -> (Failure, Cmd.none)
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
        [ column [ alignTop, alignLeft, Font.size 30, Font.bold, paddingXY 10 10, width fill, spacing 10 ]
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

viewJailInfo : Maybe Jail -> Element Msg
viewJailInfo maybeJail =
  case maybeJail of
    Nothing -> el [ padding 15] (text "Bottom text lmao")
    Just jailInfo ->
      let heading txt = el [ Font.size 26, Font.bold, paddingXY 0 15 ] (text txt)
          section els = column [ padding 15 ] els in
      column [ alignTop, spacing 5 ]
        [ section 
          [ heading "Summary"
          , text ("Current banned IPs: " ++ (String.fromInt jailInfo.currentlyBanned))
          , text ("Total banned IPs: " ++ (String.fromInt jailInfo.totalBanned))
          ]
        , section
          [ heading "Banned IPs"
          , table [ spacing 20 ]
            { data = getJailIPs jailInfo
            , columns =
              [ { header = text "IP Address"
                , width = fill
                , view = \ip -> text ip.addr }
              , { header = text "ASN"
                , width = fill
                , view = \ip -> text ip.asn }
              , { header = text "Country"
                , width = fill
                , view = \ip -> text ip.country }
              , { header = text "RIR"
                , width = fill
                , view = \ip -> text ip.rir }
              , { header = text "Unban?"
                , width = fill
                , view = \ip -> Input.button [] { onPress = Just (UnbanIP ip.addr), label = text "X" }
              }]}
          ]
        , section
          [ heading "Ignored IPs"
          , table [ spacing 20 ]
            { data = jailInfo.ignoredIPs
            , columns =
              [ { header = text "IP address"
                , width = fill
                , view = \ip -> text ip }
              , { header = text "Un-ignore?"
                , width = fill
                , view = \ip -> Input.button [] { onPress = Just (UnignoreIP ip), label = text "X"}
              }]}
          ]
        ]



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
jailDecoder =
  Decode.succeed Jail
    |> required "currently_failed" int
    |> required "total_failed" int
    |> required "file_list" (list string)
    |> required "currently_banned" int
    |> required "total_banned" int
    |> required "banned_ips" (list string)
    |> required "banned_asns" (list string)
    |> required "banned_countries" (list string)
    |> required "banned_rirs" (list string)
    |> required "ignored_ips" (list string)
    |> required "findtime" int
    |> required "bantime" int
    |> required "maxretry" int


postUnbanIP : String -> String -> Cmd Msg
postUnbanIP jail ip =
  Http.post
    { url = "http://localhost:5000/api/jails/" ++ jail ++ "/unban/" ++ ip
    , body = Http.emptyBody
    , expect = Http.expectWhatever UnbannedIP }

postUnignoreIP : String -> String -> Cmd Msg
postUnignoreIP jail ip =
  Http.post
    { url = "http://localhost:5000/api/jails/" ++ jail ++ "/delignore/" ++ ip
    , body = Http.emptyBody
    , expect = Http.expectWhatever UnignoredIP }

getJailIPs : Jail -> List IP
getJailIPs jail =
  List.map4 (\a b c d -> IP a b c d) jail.bannedIPs jail.bannedASNs jail.bannedCountries jail.bannedRIRs
