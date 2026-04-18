module Common.Network exposing (..)

import Html exposing (..)
import Http


-- Connecting

type Access
    = Loading
    | Failure Http.Error

viewAccess : Access -> Html msg
viewAccess access =
    case access of
        Loading ->
            text "Loading ..."

        Failure err ->
            text ("Failure: " ++ httpErrorToString err)

httpErrorToString : Http.Error -> String
httpErrorToString err =
    case err of
        Http.BadUrl cmt -> "Bad Url: " ++ cmt

        Http.Timeout -> "Timeout"

        Http.NetworkError -> "Network Error"

        Http.BadStatus num -> "Bad Status: " ++ String.fromInt num

        Http.BadBody cmt -> "Bad Body: " ++ cmt


-- API
type APICall reqbody response
    = Call reqbody
    | Response (Result Http.Error response)

useApi : (Access -> model) -> (reqbody -> (Result Http.Error response -> msg) -> Cmd msg) -> (APICall reqbody response -> msg) -> (response -> (model,Cmd msg)) -> APICall reqbody response -> (model, Cmd msg)
useApi connecting apiFunc msgFunc next apicall =
    case apicall of
        Call reqbody ->
            (connecting Loading, apiFunc reqbody (msgFunc << Response))

        Response result ->
            case result of
                Ok resp ->
                    next resp

                Err err ->
                    (connecting (Failure err),Cmd.none)

maybeResponse : (Access -> model) -> (response -> (model,Cmd msg)) -> Maybe response -> (model,Cmd msg)
maybeResponse connecting next result =
    case result of
        Just resp ->
            next resp

        Nothing ->
            (connecting (Failure Http.NetworkError), Cmd.none)

cmdNone : model -> (model, Cmd msg)
cmdNone mdl =
    (mdl,Cmd.none)
