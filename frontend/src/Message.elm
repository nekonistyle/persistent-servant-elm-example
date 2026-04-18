module Message exposing (..)

import Html exposing (..)
import Monocle.Lens exposing (..)

import Generated.Api exposing (..)

--

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

