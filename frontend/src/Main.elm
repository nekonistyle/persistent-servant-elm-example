module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http

import Generated.Api exposing (..)
import Common.Network exposing (..)
import Common.SimpleHtml exposing (..)
import Message exposing (..)


--MAIN

main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }

-- MODEL / MSG

type Model
    = Connecting Access
    | WritingMessage Message
    | ShowAllMessage (List Message)
    | ShowPostMessage ()

type Msg
    = ChangeMessage Message
    | AllMessage (APICall () (List Message))
    | PostMessage (APICall Message ())


-- Call API

callAllMessage : APICall () (List Message) -> (Model,Cmd Msg)
callAllMessage =
    useApi Connecting
        (\_ -> postAllMessage) AllMessage (cmdNone << ShowAllMessage)

callPostMessage : APICall Message () -> (Model,Cmd Msg)
callPostMessage =
    useApi Connecting
        postPostMessage PostMessage (cmdNone << ShowPostMessage)


-- INIT

init : () -> (Model, Cmd Msg)
init _ = callAllMessage (Call ())


-- UPDATE

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        ChangeMessage message ->
            (WritingMessage message, Cmd.none)

        AllMessage apicall ->
            callAllMessage apicall

        PostMessage apicall ->
            callPostMessage apicall

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


-- VIEW

view : Model -> Html Msg
view model =
    case model of
        Connecting access ->
            viewAccess access

        WritingMessage message ->
            writeMessage message

        ShowAllMessage messageList ->
            viewAllMessage messageList

        ShowPostMessage _ ->
            viewPostMessage


writeMessage : Message -> Html Msg
writeMessage message =
    div []
        [ simpleInput ChangeMessage messageTitleLens "title" message
        , simpleInput ChangeMessage messageContentLens "content" message
        , postMessageButton message
        ]

viewAllMessage : List Message -> Html Msg
viewAllMessage messageList =
    div []
        [ h3 [] [ text "All Messages"]
        , h5 [] [ text "Title / Content" ]
        , div [] (List.map viewMessage messageList)
        , postNewMessageButton
        ]

viewPostMessage : Html Msg
viewPostMessage =
    div []
        [ h3 [] [ text "done" ]
        , allMessageButton
        ]


postNewMessageButton : Html Msg
postNewMessageButton =
    simpleButton (ChangeMessage initMessage) "Post New Message"

postMessageButton : Message -> Html Msg
postMessageButton message =
    simpleButton (PostMessage (Call message)) "Post" 

allMessageButton : Html Msg
allMessageButton =
    simpleButton (AllMessage (Call ())) "All Messages"
