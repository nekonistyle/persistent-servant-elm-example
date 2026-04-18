module Generated.Api exposing(..)

import Json.Decode
import Json.Encode exposing (Value)
-- The following module comes from bartavelle/json-helpers
import Json.Helpers exposing (..)
import Dict exposing (Dict)
import Set
import Http
import String
import Url.Builder

type alias Message  =
   { messageTitle: String
   , messageContent: String
   }

jsonDecMessage : Json.Decode.Decoder ( Message )
jsonDecMessage =
   Json.Decode.succeed (\pmessageTitle pmessageContent -> {messageTitle = pmessageTitle, messageContent = pmessageContent})
   |> required "messageTitle" (Json.Decode.string)
   |> required "messageContent" (Json.Decode.string)

jsonEncMessage : Message -> Value
jsonEncMessage  val =
   Json.Encode.object
   [ ("messageTitle", Json.Encode.string val.messageTitle)
   , ("messageContent", Json.Encode.string val.messageContent)
   ]


postAllMessage : (Result Http.Error  ((List Message))  -> msg) -> Cmd msg
postAllMessage toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "POST"
            , headers =
                []
            , url =
                Url.Builder.crossOrigin "http://localhost:8080"
                    [ "AllMessage"
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson toMsg (Json.Decode.list (jsonDecMessage))
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

postPostMessage : Message -> (Result Http.Error  (())  -> msg) -> Cmd msg
postPostMessage body toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "POST"
            , headers =
                []
            , url =
                Url.Builder.crossOrigin "http://localhost:8080"
                    [ "PostMessage"
                    ]
                    params
            , body =
                Http.jsonBody (jsonEncMessage body)
            , expect =
                Http.expectString 
                     (\x -> case x of
                     Err e -> toMsg (Err e)
                     Ok _ -> toMsg (Ok ()))
            , timeout =
                Nothing
            , tracker =
                Nothing
            }
