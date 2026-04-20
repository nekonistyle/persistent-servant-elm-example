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

type General
    = WritingMessage Message

type alias Model
    = SimpleModel MessageModel General

type alias Msg
    = SimpleMsg MessageMsg General


-- Call API

callAllMessage : APICall () (List Message) -> (Model,Cmd Msg)
callAllMessage =
    useAPI allMessageAPI (cmdNone << APIModel << AllMessageModel)

callPostMessage : APICall Message () -> (Model,Cmd Msg)
callPostMessage =
    useAPI postMessageAPI (callAllMessage << Request)
--    useAPI postMessageAPI (cmdNone << APIModel << PostMessageModel)


-- INIT

init : () -> (Model, Cmd Msg)
init _ = callAllMessage (Request ())


-- UPDATE

update : Msg -> Model -> (Model, Cmd Msg)
update =
    let
        apiUpdate api =
            case api of
                AllMessageMsg apicall -> callAllMessage apicall

                PostMessageMsg apicall -> callPostMessage apicall

        generalUpdate general =
            case general of
                WritingMessage message ->
                    (GeneralModel (WritingMessage message),Cmd.none)
    in
        simpleUpdate apiUpdate generalUpdate


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


-- VIEW
view : Model -> Html Msg
view =
    let
        apiView api =
            case api of
                AllMessageModel messageList -> viewAllMessage messageList

                PostMessageModel _ -> viewPostMessage

        generalView general =
            case general of
                WritingMessage message -> writeMessage message
    in
        simpleView apiView generalView


writeMessage : Message -> Html Msg
writeMessage message =
    div []
        [ simpleInput (GeneralMsg << WritingMessage)
              messageTitleLens "title" message
        , simpleInput (GeneralMsg << WritingMessage)
            messageContentLens "content" message
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
    simpleButton (GeneralMsg (WritingMessage initMessage)) "Post New Message"

postMessageButton : Message -> Html Msg
postMessageButton message =
    simpleButton (APIMsg (PostMessageMsg (Request message))) "Post" 

allMessageButton : Html Msg
allMessageButton =
    simpleButton (APIMsg (AllMessageMsg (Request ()))) "All Messages"
