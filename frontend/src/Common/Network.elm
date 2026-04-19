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

type alias APISet reqbody response apiMsg
    = { api : reqbody -> (Result Http.Error response -> apiMsg) -> Cmd apiMsg
      , msg : APICall reqbody response -> apiMsg
      }

-- simple Model
type SimpleModel apiModel general
    = Connecting Access
    | APIModel apiModel
    | GeneralModel general

type SimpleMsg apiMsg general
    = APIMsg apiMsg
    | GeneralMsg general


useAPI : APISet reqbody response apiMsg
       -> (response
          -> (SimpleModel apiModel general, Cmd (SimpleMsg apiMsg general)))
       -> APICall reqbody response
       -> (SimpleModel apiModel general, Cmd (SimpleMsg apiMsg general))
useAPI apiSet next apicall =
    case apicall of
        Call reqbody ->
            (Connecting Loading
            ,Cmd.map APIMsg (apiSet.api reqbody (apiSet.msg << Response)))

        Response result ->
            case result of
                Ok resp ->
                    next resp

                Err err ->
                    (Connecting (Failure err),Cmd.none)


simpleUpdate :
    (apiMsg -> (SimpleModel apiModel general, Cmd (SimpleMsg apiMsg general)))
        -> (general
           -> (SimpleModel apiModel general, Cmd (SimpleMsg apiMsg general)))
        -> SimpleMsg apiMsg general -> SimpleModel apiModel general
        -> (SimpleModel apiModel general, Cmd (SimpleMsg apiMsg general))
simpleUpdate apiUpdate generalUpdate msg model =
    case msg of
        APIMsg apimsg -> apiUpdate apimsg

        GeneralMsg generalmsg -> generalUpdate generalmsg


simpleView : (apiModel -> Html (SimpleMsg apiMsg general)) ->
             (general -> Html (SimpleMsg apiMsg general))
                 -> SimpleModel apiModel general
                 -> Html (SimpleMsg apiMsg general)
simpleView apiView generalView model =
    case model of
        Connecting access -> viewAccess access

        APIModel apimodel -> apiView apimodel

        GeneralModel generalmodel -> generalView generalmodel


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
