port module Main exposing (main)

import Browser
import Html as H exposing (Html)
import Html.Events as E
import Html.Attributes as A
import Task
import Time as T
import Json.Encode as En
import Json.Decode as De
import Result exposing (Result(..))
import Browser.Dom exposing (focus)

-- MODEL --

type alias Model =
  -- Meaningful state
  { timer : Maybe Timer
  , log : List Entry

  -- Read from environment
  , zone : T.Zone
  , time : T.Posix

  -- Toasts
  , errors : List String
  }

type alias Timer =
  { start : T.Posix
  , note : String
  }

type alias Entry =
  { start : T.Posix
  , stop : T.Posix
  , note : String
  }


-- INIT --

init : () -> (Model, Cmd Msg)
init inital =
  let
    model =
      { timer = Nothing
      , log = []
      , zone = T.utc              -- invalid state fuck u Elm
      , time = T.millisToPosix 0  -- see above
      , errors = []
      }

    cmd = Cmd.batch
      [ Task.perform SetTimeZone T.here
      , Task.perform SetTime T.now
      ]

  in (model, cmd)


-- UPDATE --

type Msg
  = MapModel (Model -> Model)
  | SetTime T.Posix
  | SetTimeZone T.Zone
  | SetNote String
  | StartTimer
  | StopTimer
  | PushError String
  | DoNothing

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  let
    newModel =
      case msg of
        MapModel f -> f model
        SetTime newTime -> { model | time = newTime }
        SetTimeZone newZone -> { model | zone = newZone }
        SetNote newNote -> case model.timer of
            Nothing -> model
            Just timer ->
              let timer_ = { timer | note = newNote }
              in { model | timer = Just timer_ }
        StartTimer -> { model | timer = Just { start = model.time, note = "" } }
        StopTimer -> case model.timer of
            Nothing -> model
            Just timer ->
              { model | timer = Nothing
                      , log =
                              { start = timer.start
                              , stop = model.time
                              , note = timer.note
                              }
                              :: model.log
                      }
        PushError e -> { model | errors = model.errors ++ [e] }
        DoNothing -> model

    saveCmd = save <| encodeModel newModel

    focusCmd = case msg of
      StartTimer -> Task.attempt (\_ -> DoNothing) (focus "note-input")
      _ -> Cmd.none

  in (newModel, Cmd.batch [ saveCmd, focusCmd ])


-- VIEW --

view : Model -> Html Msg
view model =
  let

    modelView =
      H.div
        [ A.class "elm" ]
        [ H.div
            [ A.class "errors" ]
            ( model.errors |> List.map viewError )
        , timerView
        , logView
        ]

    viewError error =
      H.pre [ A.class "errors-error" ] [ H.text error ]

    timerView =
      H.div [ A.class "timer" ]
        <| case model.timer of
          Just timer ->
            [ buttonView, counterView timer.start, noteView timer.note ]
          Nothing ->
            [ buttonView ]

    buttonView =
      let
        timerIdle = case model.timer of
          Just _ -> False
          Nothing -> True
      in
      H.button
        [ E.onClick <| if timerIdle then StartTimer else StopTimer
        , A.class "timer-button"
        ]
        [ H.text <| if timerIdle then "Start" else "Stop" ]

    counterView startTime =
      H.div
        [ A.class "timer-counter" ]
        [ H.text <| fmtElapsed <| getElapsed startTime model.time ]

    noteView note =
      H.input
        [ E.onInput SetNote
        , A.value note
        , A.class "timer-note"
        , A.placeholder "set a description"
        , A.id "note-input"
        ]
        [ ]

    logView =
      H.div [ A.class "log" ] <| (
        model.log
        |> groupBy (\entry -> fmtDay model.zone entry.start)
        |> List.concatMap (\(day, entries) ->
              (H.p
                [ A.class "log-header" ]
                [ H.span [ A.class "log-entry-note" ] [ H.text day ]
                , H.span
                    [ A.class "log-entry-time" ]
                    [ H.text <| fmtElapsed
                      ( entries |> List.map (\entry -> getElapsed entry.start entry.stop) |> List.sum )
                    ]
                ]
              )
              :: (entries |> List.map viewEntry)
              ))

    viewEntry entry =
      H.p [ A.class "log-entry" ]
        [ H.span [ A.class "log-entry-note" ] [ if entry.note /= "" then H.text entry.note
                                                else H.span [ A.class "subtle" ] [ H.text "no description" ]
                                              ]
        , H.span [ A.class "log-entry-time" ] [ H.text <| fmtElapsed <| getElapsed entry.start entry.stop ]
        ]

  in
    modelView

groupBy : (a -> k) -> List a -> List (k, List a)
groupBy keyf ar = case ar of
  [] -> []
  x :: xs -> case groupBy keyf xs of
    [] -> [(keyf x, [x])]
    (kr, rs) :: rest ->
      let kx = keyf x in
      if kx == kr
      then (kr, x :: rs) :: rest
      else (kx, [x]) :: (kr, rs) :: rest

fmtDay : T.Zone -> T.Posix -> String
fmtDay z t =
  let
    y = String.fromInt <| T.toYear z t

    m = case T.toMonth z t of
          T.Jan -> "Jan"
          T.Feb -> "Feb"
          T.Mar -> "Mar"
          T.Apr -> "Apr"
          T.May -> "May"
          T.Jun -> "Jun"
          T.Jul -> "Jul"
          T.Aug -> "Aug"
          T.Sep -> "Sep"
          T.Oct -> "Oct"
          T.Nov -> "Nov"
          T.Dec -> "Dec"

    d = String.fromInt <| T.toDay z t

  in m ++ " " ++ d ++ ", " ++ y

getElapsed : T.Posix -> T.Posix -> Int
getElapsed start stop = T.posixToMillis stop - T.posixToMillis start

fmtElapsed : Int -> String
fmtElapsed ms =
  let h = ms // (1000 * 60 * 60)
      m = modBy 60 (ms // (1000 * 60))
      s = modBy 60 (ms // 1000)
      padShow n = (if n < 10 then "0" else "") ++ String.fromInt n
  in
    String.fromInt h ++ "h " ++ padShow m ++ "m " ++ padShow s ++ "s"

-- IO --

encodeModel : Model -> En.Value
encodeModel =
  let
    impl = \{ timer, log } ->
      En.object
        [ ( "timer", case timer of
              Nothing -> En.null
              Just { start, note } -> En.object
                [ ( "start", En.int <| T.posixToMillis start )
                , ( "note", En.string <| note )
                ]
          )
        , ( "log", En.list encodeEntry log )
        ]

    encodeEntry : Entry -> En.Value
    encodeEntry { start, stop, note } =
      En.object
        [ ( "start", En.int <| T.posixToMillis start )
        , ( "stop", En.int <| T.posixToMillis stop )
        , ( "note", En.string note )
        ]

  in impl

decodeModel : En.Value -> Result De.Error { timer : Maybe Timer, log : List Entry }
decodeModel =
  let
    decoder =
      De.map2 (\timer log -> { timer = timer, log = log })
        (De.field "timer" <| De.nullable <| De.map2 (\start note -> { start = start, note = note })
                                                    (De.field "start" <| De.map T.millisToPosix De.int)
                                                    (De.field "note" De.string))
        (De.field "log" <| De.list <| De.map3 (\start stop note -> { start = start, stop = stop, note = note })
                                              (De.field "start" <| De.map T.millisToPosix De.int)
                                              (De.field "stop" <| De.map T.millisToPosix De.int)
                                              (De.field "note" De.string))

   in De.decodeValue decoder


port save : En.Value -> Cmd msg
port load : (En.Value -> msg) -> Sub msg

-- MAIN --

main = Browser.element
  { init = init
  , view = view
  , update = update
  , subscriptions = \_ ->
      Sub.batch
      [ T.every 1000 SetTime
      , load <|
          \json -> case decodeModel json of
            Ok { timer, log } -> MapModel <| \m -> { m | timer = timer, log = log }
            Err err -> PushError "Saved data is corrupt"
      ]
  }
