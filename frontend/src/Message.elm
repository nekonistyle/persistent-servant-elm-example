module Message exposing (..)

import Html exposing (..)
import Monocle.Lens exposing (..)

import Generated.Api exposing (..)

import Common.Network exposing (..)

-- init & view
initMessage : Message
initMessage = Message "" ""

viewMessage : Message -> Html msg
viewMessage message =
    div []
        [ text ( message.messageTitle ++ " / " ++ message.messageContent )
        ]

-- Lens
messageTitleLens : Lens Message String
messageTitleLens = Lens .messageTitle (\s m -> { m | messageTitle = s })

messageContentLens : Lens Message String
messageContentLens = Lens .messageContent (\s m -> { m | messageContent = s })

-- Model & Msg
type MessageModel
    = AllMessageModel (List Message)
    | PostMessageModel ()

type MessageMsg
    = AllMessageMsg (APICall () (List Message))
    | PostMessageMsg (APICall Message ())

-- Msg & API
allMessageAPI : APISet () (List Message) MessageMsg
allMessageAPI = APISet (\_ -> postAllMessage) AllMessageMsg

postMessageAPI : APISet Message () MessageMsg
postMessageAPI = APISet postPostMessage PostMessageMsg
