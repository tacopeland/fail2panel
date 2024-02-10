module Main exposing (..)

import Config exposing (apiUrl)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Input as Input
import Http
import Html exposing (Html)
import Html.Events
import Json.Decode as Decode exposing (Decoder, field, int, string, list)
import Json.Decode.Pipeline exposing (required, hardcoded)
import List exposing (length, repeat, any)



-- MAIN


main =
  Browser.document
    { init = init
    , subscriptions = subscriptions
    , update = update
    , view = view
    }



-- EVENTS

onEnter : msg -> Element.Attribute msg
onEnter msg =
  htmlAttribute (Html.Events.on "keyup"
    (Decode.field "key" Decode.string
      |> Decode.andThen
        (\key ->
          if key == "Enter" then
            Decode.succeed msg
          else
            Decode.fail "Not the enter key"
        )
    )
  )



-- MODEL


type Model
  = Failure
  | JailsPage JailsPageModel

type alias JailsPageModel =
  { jails : Jails
  , activeJail : Maybe String
  , jailInfo : Maybe Jail
  , f2bVersion : Maybe String
  , logTarget : Maybe String
  , dbFile : Maybe String
  , dbPurgeAge : Maybe Int
  }

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
  , banIPInput : String
  , ignoredIPs : List String
  , ignoreIPInput : String
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

type JailInfoInput =
    InputBanIP 
  | InputIgnoreIP
  | InputFindTime
  | InputBanTime
  | InputMaxRetry

initialModel = JailsPageModel [] Nothing Nothing Nothing Nothing Nothing Nothing


init : () -> (Model, Cmd Msg)
init _ = (JailsPage initialModel, getJails)



-- UPDATE


type Msg
  = GotJails (Result Http.Error Jails)
  | GotVersion (Result Http.Error String)
  | GotLogTarget (Result Http.Error String)
  | GotDbFile (Result Http.Error String)
  | GotDbPurgeAge (Result Http.Error Int)
  | DeselectJails
  | SelectedJail String
  | UnbanIP String
  | UpdatedJail (Result Http.Error ())
  | UnignoreIP String
  | GotJail String (Result Http.Error Jail)
  | UpdateText JailInfoInput String
  | EnterWasPressed JailInfoInput


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotJails result ->
      case result of
        Ok jailList ->
          case model of
            Failure -> (JailsPage { initialModel | jails = jailList }, getVersion)
            JailsPage jpModel -> (JailsPage { jpModel | jails = jailList }, getVersion)
        Err _ -> (Failure, Cmd.none)
    GotVersion result ->
      case result of
        Ok version ->
          case model of
            Failure -> (model, Cmd.none)
            JailsPage jpModel -> (JailsPage { jpModel | f2bVersion = Just version }, getLogTarget)
        Err _ -> (Failure, Cmd.none)
    GotLogTarget result ->
      case result of
        Ok logTarget ->
          case model of
            Failure -> (model, Cmd.none)
            JailsPage jpModel -> (JailsPage { jpModel | logTarget = Just logTarget }, getDbFile)
        Err _ -> (Failure, Cmd.none)
    GotDbFile result ->
      case result of
        Ok dbFile ->
          case model of
            Failure -> (Failure, Cmd.none)
            JailsPage jpModel -> (JailsPage { jpModel | dbFile = Just dbFile }, getDbPurgeAge)
        Err _ -> (Failure, Cmd.none)
    GotDbPurgeAge result ->
      case result of
        Ok dbPurgeAge ->
          case model of
            Failure -> (Failure, Cmd.none)
            JailsPage jpModel -> (JailsPage { jpModel | dbPurgeAge = Just dbPurgeAge }, Cmd.none)
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
    UpdatedJail result -> 
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
    GotJail jailName result ->
      case result of
        Ok jail -> 
          case model of
            Failure -> init ()
            JailsPage jpModel -> (JailsPage { jpModel | jailInfo = Just jail }, Cmd.none)
        Err _ -> (Failure, Cmd.none)
    UpdateText input txt ->
      case model of
        Failure -> init ()
        JailsPage jpModel ->
          case jpModel.jailInfo of
            Nothing -> (model, Cmd.none)
            Just jailInfo ->
              case input of
                InputBanIP ->
                  (JailsPage { jpModel | jailInfo = Just { jailInfo | banIPInput = txt }}, Cmd.none)
                InputIgnoreIP ->
                  (JailsPage { jpModel | jailInfo = Just { jailInfo | ignoreIPInput = txt }}, Cmd.none)
                InputFindTime ->
                  case String.toInt txt of
                    Nothing ->
                      (model, Cmd.none)
                    Just num ->
                      (JailsPage { jpModel | jailInfo = Just { jailInfo | findTime = num }}, Cmd.none)
                InputBanTime ->
                  case String.toInt txt of
                    Nothing ->
                      (model, Cmd.none)
                    Just num ->
                      (JailsPage { jpModel | jailInfo = Just { jailInfo | banTime = num }}, Cmd.none)
                InputMaxRetry ->
                  case String.toInt txt of
                    Nothing ->
                      (model, Cmd.none)
                    Just num ->
                      (JailsPage { jpModel | jailInfo = Just { jailInfo | maxRetry = num }}, Cmd.none)
    EnterWasPressed input ->
      case model of
        Failure -> init ()
        JailsPage jpModel ->
          case jpModel.jailInfo of
            Nothing -> (model, Cmd.none)
            Just jailInfo ->
              case jpModel.activeJail of
                Nothing -> (model, Cmd.none)
                Just activeJail ->
                  case input of
                    InputBanIP ->
                      (model, postBanIP activeJail jailInfo.banIPInput)
                    InputIgnoreIP ->
                      (model, postIgnoreIP activeJail jailInfo.ignoreIPInput)
                    InputFindTime ->
                      (model, postFindTime activeJail (String.fromInt jailInfo.findTime))
                    InputBanTime ->
                      (model, postBanTime activeJail (String.fromInt jailInfo.banTime))
                    InputMaxRetry ->
                      (model, postMaxRetry activeJail (String.fromInt jailInfo.maxRetry))
      

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
        , column
          [ alignTop, alignLeft, height fill, width fill, scrollbarY ]
          [viewJailInfo jpModel]
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

viewJailInfo : JailsPageModel -> Element Msg
viewJailInfo model =
  let heading txt = el [ Font.size 26, Font.bold, paddingXY 0 15 ] (text txt)
      section els = column [ padding 15, spacing 10 ] els
      txtInput input modelField label =
        Input.text
          [ onEnter (EnterWasPressed input), spacing 10 ]
          { onChange = (UpdateText input)
          , text = modelField
          , placeholder = Nothing
          , label = Input.labelLeft [] (text label)
          }
      xImg = image [ height (px 16), width (px 16) ] { src = "x.png", description = "" }
      maybeTxt txt = case txt of
        Nothing -> "..."
        Just strTxt -> strTxt
      maybeIntToTxt int = case int of
        Nothing -> "..."
        Just innerInt -> String.fromInt innerInt
  in
  case model.jailInfo of
    Nothing -> 
      column [ alignTop, spacing 5 ]
        [ section
          [ heading "fail2ban web panel"
          , text ("Currently running on fail2ban version " ++ (maybeTxt model.f2bVersion))
          , text ("Logging to " ++ (maybeTxt model.logTarget))
          , text ("Storing persistent data in " ++
                  (maybeTxt model.dbFile) ++
                  " for a maximum of " ++
                  (maybeIntToTxt model.dbPurgeAge) ++
                  " seconds.")
          ]
        ]
    Just jailInfo ->
      column [ alignTop, spacing 5 ]
        [ section 
          [ heading "Summary"
          , text ("Current failed IPs: " ++ (String.fromInt jailInfo.currentlyFailed))
          , text ("Total failed IPs: " ++ (String.fromInt jailInfo.totalFailed))
          , text ("Current banned IPs: " ++ (String.fromInt jailInfo.currentlyBanned))
          , text ("Total banned IPs: " ++ (String.fromInt jailInfo.totalBanned))
          , text ("Currently watching logs: " ++ (String.join ", " jailInfo.fileList))
          ]
        , section
          [ heading "Running config"
          , txtInput InputFindTime (String.fromInt jailInfo.findTime) "Find time: "
          , txtInput InputBanTime (String.fromInt jailInfo.banTime) "Ban time: "
          , txtInput InputMaxRetry (String.fromInt jailInfo.maxRetry) "Max retries: "
          ]
        , section
          [ heading "Banned IPs"
          , table
            [ spacing 20
            , paddingEach { bottom = 15, left = 0, right = 0, top = 0 }
            , Border.widthEach { bottom = 1, left = 0, right = 0, top = 0}
            , Border.color (rgb 0.8 0.8 0.8)
            ]
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
              , { header = Element.none
                , width = fill
                , view = \ip -> Input.button [] { onPress = Just (UnbanIP ip.addr), label = xImg }
              }]}
          , if
              any (\l -> l == ["error"]) [jailInfo.bannedASNs, jailInfo.bannedCountries, jailInfo.bannedRIRs]
            then
              el [Font.italic] (text ("To get ASN, Country, and RIR, install dnspython where fail2ban can see it."))
            else
              none
          , txtInput InputBanIP jailInfo.banIPInput "Ban IP: "
          ]
        , section
          [ heading "Ignored IPs"
          , table
            [ spacing 20
            , paddingEach { bottom = 15, left = 0, right = 0, top = 0 }
            , Border.widthEach { bottom = 1, left = 0, right = 0, top = 0}
            , Border.color (rgb 0.8 0.8 0.8)
            ]
            { data = jailInfo.ignoredIPs
            , columns =
              [ { header = text "IP address"
                , width = fill
                , view = \ip -> text ip }
              , { header = Element.none
                , width = fill
                , view = \ip -> Input.button [] { onPress = Just (UnignoreIP ip), label = xImg }
              }]}
          , txtInput InputIgnoreIP jailInfo.ignoreIPInput "Ignore IP: "
          ]
        ]



-- HTTP


getJails : Cmd Msg
getJails =
  Http.get
    { url = apiUrl ++ "/jails"
    , expect = Http.expectJson GotJails (field "jails" (list string))
    }

getVersion : Cmd Msg
getVersion =
  Http.get
    { url = apiUrl ++ "/version"
    , expect = Http.expectJson GotVersion (field "version" string)
    }

getLogTarget : Cmd Msg
getLogTarget =
  Http.get
    { url = apiUrl ++ "/logtarget"
    , expect = Http.expectJson GotLogTarget (field "logtarget" string)
    }

getDbFile : Cmd Msg
getDbFile =
  Http.get
    { url = apiUrl ++ "/dbfile"
    , expect = Http.expectJson GotDbFile (field "dbfile" string)
    }

getDbPurgeAge : Cmd Msg
getDbPurgeAge =
  Http.get
    { url = apiUrl ++ "/dbpurgeage"
    , expect = Http.expectJson GotDbPurgeAge (field "dbpurgeage" int)
    }

getJail : String -> Cmd Msg
getJail jail =
  Http.get
    { url = apiUrl ++ "/jails/" ++ jail
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
    |> hardcoded ""
    |> required "ignored_ips" (list string)
    |> hardcoded ""
    |> required "findtime" int
    |> required "bantime" int
    |> required "maxretry" int


postUnbanIP : String -> String -> Cmd Msg
postUnbanIP jail ip =
  Http.post
    { url = apiUrl ++ "/jails/" ++ jail ++ "/unban/" ++ ip
    , body = Http.emptyBody
    , expect = Http.expectWhatever UpdatedJail }

postBanIP : String -> String -> Cmd Msg
postBanIP jail ip =
  Http.post
    { url = apiUrl ++ "/jails/" ++ jail ++ "/ban/" ++ ip
    , body = Http.emptyBody
    , expect = Http.expectWhatever UpdatedJail }

postUnignoreIP : String -> String -> Cmd Msg
postUnignoreIP jail ip =
  Http.post
    { url = apiUrl ++ "/jails/" ++ jail ++ "/delignore/" ++ ip
    , body = Http.emptyBody
    , expect = Http.expectWhatever UpdatedJail }

postIgnoreIP : String -> String -> Cmd Msg
postIgnoreIP jail ip =
  Http.post
    { url = apiUrl ++ "/jails/" ++ jail ++ "/addignore/" ++ ip
    , body = Http.emptyBody
    , expect = Http.expectWhatever UpdatedJail }

postFindTime : String -> String -> Cmd Msg
postFindTime jail time =
  Http.post
    { url = apiUrl ++ "/jails/" ++ jail ++ "/findtime/" ++ time
    , body = Http.emptyBody
    , expect = Http.expectWhatever UpdatedJail }

postBanTime : String -> String -> Cmd Msg
postBanTime jail time =
  Http.post
    { url = apiUrl ++ "/jails/" ++ jail ++ "/bantime/" ++ time
    , body = Http.emptyBody
    , expect = Http.expectWhatever UpdatedJail }

postMaxRetry : String -> String -> Cmd Msg
postMaxRetry jail amt =
  Http.post
    { url = apiUrl ++ "/jails/" ++ jail ++ "/maxretry/" ++ amt
    , body = Http.emptyBody
    , expect = Http.expectWhatever UpdatedJail }

getJailIPs : Jail -> List IP
getJailIPs jail =
  let fillSpace l = l ++ repeat (length jail.bannedIPs - length l) ""
      bannedASNs = if jail.bannedASNs == ["error"] then fillSpace [] else fillSpace jail.bannedASNs
      bannedCountries = if jail.bannedCountries == ["error"] then fillSpace [] else fillSpace jail.bannedCountries
      bannedRIRs = if jail.bannedRIRs == ["error"] then fillSpace [] else fillSpace jail.bannedRIRs
  in
  List.map4 (\a b c d -> IP a b c d) jail.bannedIPs bannedASNs bannedCountries bannedRIRs
