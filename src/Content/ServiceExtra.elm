module Content.ServiceExtra exposing (Callbacks, Msg(..), callbacks, invokeRetrieveTree, invokeRetrieveWithContainer, invokeRetrieveWithContainerBySlug, retrieveTreeTask, retrieveWithContainerBySlugTask, retrieveWithContainerTask, routes, update)

import Http
import Json.Decode as Decode exposing (..)
import Json.Encode as Encode exposing (..)
import Model exposing (..)
import Platform.Cmd exposing (Cmd)
import Result
import Task exposing (Task)


type Msg
    = RetrieveWithContainer (Result.Result Http.Error Model.Content)
    | RetrieveWithContainerBySlug (Result.Result Http.Error Model.Content)
    | RetrieveTree (Result.Result Http.Error Model.Content)


invokeRetrieveWithContainer : String -> (Msg -> msg) -> String -> Cmd msg
invokeRetrieveWithContainer root msg id =
    retrieveWithContainerTask root id
        |> Http.send RetrieveWithContainer
        |> Cmd.map msg


invokeRetrieveWithContainerBySlug : String -> (Msg -> msg) -> String -> Cmd msg
invokeRetrieveWithContainerBySlug root msg id =
    retrieveWithContainerBySlugTask root id
        |> Http.send RetrieveWithContainerBySlug
        |> Cmd.map msg


invokeRetrieveTree : String -> (Msg -> msg) -> Cmd msg
invokeRetrieveTree root msg =
    retrieveTreeTask root
        |> Http.send RetrieveTree
        |> Cmd.map msg


type alias Callbacks model msg =
    { retrieveWithContainer : Model.Content -> model -> ( model, Cmd msg )
    , retrieveWithContainerError : Http.Error -> model -> ( model, Cmd msg )
    , retrieveWithContainerBySlug : Model.Content -> model -> ( model, Cmd msg )
    , retrieveWithContainerBySlugError : Http.Error -> model -> ( model, Cmd msg )
    , retrieveTree : Model.Content -> model -> ( model, Cmd msg )
    , retrieveTreeError : Http.Error -> model -> ( model, Cmd msg )
    , error : Http.Error -> model -> ( model, Cmd msg )
    }


callbacks : Callbacks model msg
callbacks =
    { retrieveWithContainer = \_ -> \model -> ( model, Cmd.none )
    , retrieveWithContainerError = \_ -> \model -> ( model, Cmd.none )
    , retrieveWithContainerBySlug = \_ -> \model -> ( model, Cmd.none )
    , retrieveWithContainerBySlugError = \_ -> \model -> ( model, Cmd.none )
    , retrieveTree = \_ -> \model -> ( model, Cmd.none )
    , retrieveTreeError = \_ -> \model -> ( model, Cmd.none )
    , error = \_ -> \model -> ( model, Cmd.none )
    }


update : Callbacks model msg -> Msg -> model -> ( model, Cmd msg )
update callbacks action model =
    case Debug.log "content.api" action of
        RetrieveWithContainer result ->
            case result of
                Ok content ->
                    callbacks.retrieveWithContainer content model

                Err httpError ->
                    let
                        ( modelSpecific, cmdSpecific ) =
                            callbacks.retrieveWithContainerError httpError model

                        ( modelGeneral, cmdGeneral ) =
                            callbacks.error httpError modelSpecific
                    in
                    ( modelGeneral, Cmd.batch [ cmdSpecific, cmdGeneral ] )

        RetrieveWithContainerBySlug result ->
            case result of
                Ok content ->
                    callbacks.retrieveWithContainerBySlug content model

                Err httpError ->
                    let
                        ( modelSpecific, cmdSpecific ) =
                            callbacks.retrieveWithContainerBySlugError httpError model

                        ( modelGeneral, cmdGeneral ) =
                            callbacks.error httpError modelSpecific
                    in
                    ( modelGeneral, Cmd.batch [ cmdSpecific, cmdGeneral ] )

        RetrieveTree result ->
            case result of
                Ok content ->
                    callbacks.retrieveTree content model

                Err httpError ->
                    let
                        ( modelSpecific, cmdSpecific ) =
                            callbacks.retrieveTreeError httpError model

                        ( modelGeneral, cmdGeneral ) =
                            callbacks.error httpError modelSpecific
                    in
                    ( modelGeneral, Cmd.batch [ cmdSpecific, cmdGeneral ] )


routes root =
    { retrieveWithContainer = root ++ "content/full/"
    , retrieveWithContainerBySlug = root ++ "content/full/slug/"
    , retrieveTree = root ++ "content/tree"
    }


retrieveWithContainerTask : String -> String -> Http.Request Content
retrieveWithContainerTask root id =
    Http.request
        { method = "GET"
        , headers = []
        , url = (routes root |> .retrieveWithContainer) ++ id
        , body = Http.emptyBody
        , expect = Http.expectJson contentDecoder
        , timeout = Nothing
        , withCredentials = False
        }


retrieveWithContainerBySlugTask : String -> String -> Http.Request Content
retrieveWithContainerBySlugTask root id =
    Http.request
        { method = "GET"
        , headers = []
        , url = (routes root |> .retrieveWithContainerBySlug) ++ id
        , body = Http.emptyBody
        , expect = Http.expectJson contentDecoder
        , timeout = Nothing
        , withCredentials = False
        }


retrieveTreeTask : String -> Http.Request Content
retrieveTreeTask root =
    Http.request
        { method = "GET"
        , headers = []
        , url = routes root |> .retrieveTree
        , body = Http.emptyBody
        , expect = Http.expectJson contentDecoder
        , timeout = Nothing
        , withCredentials = False
        }
