module ContentType.Service exposing (Callbacks, Msg(..), callbacks, createTask, deleteTask, findAllTask, findByExampleTask, invokeCreate, invokeDelete, invokeFindAll, invokeFindByExample, invokeRetrieve, invokeUpdate, retrieveTask, routes, update, updateTask)

import Http
import Http.Decorators
import Json.Decode as Decode exposing (..)
import Json.Encode as Encode exposing (..)
import Model exposing (..)
import Platform.Cmd exposing (Cmd)
import Result
import Task exposing (Task)


type Msg
    = FindAll (Result.Result Http.Error (List Model.ContentType))
    | FindByExample (Result.Result Http.Error (List Model.ContentType))
    | Create (Result.Result Http.Error Model.ContentType)
    | Retrieve (Result.Result Http.Error Model.ContentType)
    | Update (Result.Result Http.Error Model.ContentType)
    | Delete (Result.Result Http.Error String)


invokeFindAll : String -> (Msg -> msg) -> Cmd msg
invokeFindAll root msg =
    findAllTask root
        |> Http.send FindAll
        |> Cmd.map msg


invokeFindByExample : String -> (Msg -> msg) -> Model.ContentType -> Cmd msg
invokeFindByExample root msg example =
    findByExampleTask root example
        |> Http.send FindByExample
        |> Cmd.map msg


invokeCreate : String -> (Msg -> msg) -> Model.ContentType -> Cmd msg
invokeCreate root msg model =
    createTask root model
        |> Http.send Create
        |> Cmd.map msg


invokeRetrieve : String -> (Msg -> msg) -> String -> Cmd msg
invokeRetrieve root msg id =
    retrieveTask root id
        |> Http.send Retrieve
        |> Cmd.map msg


invokeUpdate : String -> (Msg -> msg) -> String -> Model.ContentType -> Cmd msg
invokeUpdate root msg id model =
    updateTask root id model
        |> Http.send Update
        |> Cmd.map msg


invokeDelete : String -> (Msg -> msg) -> String -> Cmd msg
invokeDelete root msg id =
    let
        delete result =
            Delete <| Result.map (\() -> id) result
    in
    deleteTask root id
        |> Http.send delete
        |> Cmd.map msg


type alias Callbacks model msg =
    { findAll : List Model.ContentType -> model -> ( model, Cmd msg )
    , findAllError : Http.Error -> model -> ( model, Cmd msg )
    , findByExample : List Model.ContentType -> model -> ( model, Cmd msg )
    , findByExampleError : Http.Error -> model -> ( model, Cmd msg )
    , create : Model.ContentType -> model -> ( model, Cmd msg )
    , createError : Http.Error -> model -> ( model, Cmd msg )
    , retrieve : Model.ContentType -> model -> ( model, Cmd msg )
    , retrieveError : Http.Error -> model -> ( model, Cmd msg )
    , update : Model.ContentType -> model -> ( model, Cmd msg )
    , updateError : Http.Error -> model -> ( model, Cmd msg )
    , delete : String -> model -> ( model, Cmd msg )
    , deleteError : Http.Error -> model -> ( model, Cmd msg )
    , error : Http.Error -> model -> ( model, Cmd msg )
    }


callbacks : Callbacks model msg
callbacks =
    { findAll = \_ -> \model -> ( model, Cmd.none )
    , findAllError = \_ -> \model -> ( model, Cmd.none )
    , findByExample = \_ -> \model -> ( model, Cmd.none )
    , findByExampleError = \_ -> \model -> ( model, Cmd.none )
    , create = \_ -> \model -> ( model, Cmd.none )
    , createError = \_ -> \model -> ( model, Cmd.none )
    , retrieve = \_ -> \model -> ( model, Cmd.none )
    , retrieveError = \_ -> \model -> ( model, Cmd.none )
    , update = \_ -> \model -> ( model, Cmd.none )
    , updateError = \_ -> \model -> ( model, Cmd.none )
    , delete = \_ -> \model -> ( model, Cmd.none )
    , deleteError = \_ -> \model -> ( model, Cmd.none )
    , error = \_ -> \model -> ( model, Cmd.none )
    }


update : Callbacks model msg -> Msg -> model -> ( model, Cmd msg )
update callbacks action model =
    case Debug.log "contentType.api" action of
        FindAll result ->
            case result of
                Ok contentType ->
                    callbacks.findAll contentType model

                Err httpError ->
                    let
                        ( modelSpecific, cmdSpecific ) =
                            callbacks.findAllError httpError model

                        ( modelGeneral, cmdGeneral ) =
                            callbacks.error httpError modelSpecific
                    in
                    ( modelGeneral, Cmd.batch [ cmdSpecific, cmdGeneral ] )

        FindByExample result ->
            case result of
                Ok contentType ->
                    callbacks.findByExample contentType model

                Err httpError ->
                    let
                        ( modelSpecific, cmdSpecific ) =
                            callbacks.findByExampleError httpError model

                        ( modelGeneral, cmdGeneral ) =
                            callbacks.error httpError modelSpecific
                    in
                    ( modelGeneral, Cmd.batch [ cmdSpecific, cmdGeneral ] )

        Create result ->
            case result of
                Ok contentType ->
                    callbacks.create contentType model

                Err httpError ->
                    let
                        ( modelSpecific, cmdSpecific ) =
                            callbacks.createError httpError model

                        ( modelGeneral, cmdGeneral ) =
                            callbacks.error httpError modelSpecific
                    in
                    ( modelGeneral, Cmd.batch [ cmdSpecific, cmdGeneral ] )

        Retrieve result ->
            case result of
                Ok contentType ->
                    callbacks.retrieve contentType model

                Err httpError ->
                    let
                        ( modelSpecific, cmdSpecific ) =
                            callbacks.retrieveError httpError model

                        ( modelGeneral, cmdGeneral ) =
                            callbacks.error httpError modelSpecific
                    in
                    ( modelGeneral, Cmd.batch [ cmdSpecific, cmdGeneral ] )

        Update result ->
            case result of
                Ok contentType ->
                    callbacks.update contentType model

                Err httpError ->
                    let
                        ( modelSpecific, cmdSpecific ) =
                            callbacks.updateError httpError model

                        ( modelGeneral, cmdGeneral ) =
                            callbacks.error httpError modelSpecific
                    in
                    ( modelGeneral, Cmd.batch [ cmdSpecific, cmdGeneral ] )

        Delete result ->
            case result of
                Ok id ->
                    callbacks.delete id model

                Err httpError ->
                    let
                        ( modelSpecific, cmdSpecific ) =
                            callbacks.deleteError httpError model

                        ( modelGeneral, cmdGeneral ) =
                            callbacks.error httpError modelSpecific
                    in
                    ( modelGeneral, Cmd.batch [ cmdSpecific, cmdGeneral ] )


routes root =
    { findAll = root ++ "contentType"
    , findByExample = root ++ "contentType/example"
    , create = root ++ "contentType"
    , retrieve = root ++ "contentType/"
    , update = root ++ "contentType/"
    , delete = root ++ "contentType/"
    }


findAllTask : String -> Http.Request (List ContentType)
findAllTask root =
    Http.request
        { method = "GET"
        , headers = []
        , url = routes root |> .findAll
        , body = Http.emptyBody
        , expect = Http.expectJson (Decode.list contentTypeDecoder)
        , timeout = Nothing
        , withCredentials = False
        }


findByExampleTask : String -> ContentType -> Http.Request (List ContentType)
findByExampleTask root model =
    Http.request
        { method = "POST"
        , headers = []
        , url = routes root |> .findByExample
        , body = Http.jsonBody <| contentTypeEncoder model
        , expect = Http.expectJson (Decode.list contentTypeDecoder)
        , timeout = Nothing
        , withCredentials = False
        }


createTask : String -> ContentType -> Http.Request ContentType
createTask root model =
    Http.request
        { method = "POST"
        , headers = []
        , url = routes root |> .create
        , body = Http.jsonBody <| contentTypeEncoder model
        , expect = Http.expectJson contentTypeDecoder
        , timeout = Nothing
        , withCredentials = False
        }


retrieveTask : String -> String -> Http.Request ContentType
retrieveTask root id =
    Http.request
        { method = "GET"
        , headers = []
        , url = (routes root |> .retrieve) ++ id
        , body = Http.emptyBody
        , expect = Http.expectJson contentTypeDecoder
        , timeout = Nothing
        , withCredentials = False
        }


updateTask : String -> String -> ContentType -> Http.Request ContentType
updateTask root id model =
    Http.request
        { method = "PUT"
        , headers = []
        , url = (routes root |> .update) ++ id
        , body = Http.jsonBody <| contentTypeEncoder model
        , expect = Http.expectJson contentTypeDecoder
        , timeout = Nothing
        , withCredentials = False
        }


deleteTask : String -> String -> Http.Request ()
deleteTask root id =
    Http.request
        { method = "DELETE"
        , headers = []
        , url = (routes root |> .delete) ++ id
        , body = Http.emptyBody
        , expect = Http.expectStringResponse (\_ -> Ok ())
        , timeout = Nothing
        , withCredentials = False
        }
