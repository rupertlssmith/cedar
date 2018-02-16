module Relationship.Service exposing (..)

import Platform.Cmd exposing (Cmd)
import Result
import Http
import Http
import Http.Decorators
import Json.Decode as Decode exposing (..)
import Json.Encode as Encode exposing (..)
import Task exposing (Task)
import Model exposing (..)


type Msg
    = FindAll (Result.Result Http.Error (List Model.Relationship))
    | FindByExample (Result.Result Http.Error (List Model.Relationship))
    | Create (Result.Result Http.Error Model.Relationship)
    | Retrieve (Result.Result Http.Error Model.Relationship)
    | Update (Result.Result Http.Error Model.Relationship)
    | Delete (Result.Result Http.Error String)


invokeFindAll : String -> (Msg -> msg) -> Cmd msg
invokeFindAll root msg =
    findAllTask root
        |> Http.send FindAll
        |> Cmd.map msg


invokeFindByExample : String -> (Msg -> msg) -> Model.Relationship -> Cmd msg
invokeFindByExample root msg example =
    findByExampleTask root example
        |> Http.send FindByExample
        |> Cmd.map msg


invokeCreate : String -> (Msg -> msg) -> Model.Relationship -> Cmd msg
invokeCreate root msg model =
    createTask root model
        |> Http.send Create
        |> Cmd.map msg


invokeRetrieve : String -> (Msg -> msg) -> String -> Cmd msg
invokeRetrieve root msg id =
    retrieveTask root id
        |> Http.send Retrieve
        |> Cmd.map msg


invokeUpdate : String -> (Msg -> msg) -> String -> Model.Relationship -> Cmd msg
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
    { findAll : List Model.Relationship -> model -> ( model, Cmd msg )
    , findAllError : Http.Error -> model -> ( model, Cmd msg )
    , findByExample : List Model.Relationship -> model -> ( model, Cmd msg )
    , findByExampleError : Http.Error -> model -> ( model, Cmd msg )
    , create : Model.Relationship -> model -> ( model, Cmd msg )
    , createError : Http.Error -> model -> ( model, Cmd msg )
    , retrieve : Model.Relationship -> model -> ( model, Cmd msg )
    , retrieveError : Http.Error -> model -> ( model, Cmd msg )
    , update : Model.Relationship -> model -> ( model, Cmd msg )
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
    case action of
        FindAll result ->
            (case result of
                Ok relationship ->
                    callbacks.findAll relationship model

                Err httpError ->
                    let
                        ( modelSpecific, cmdSpecific ) =
                            callbacks.findAllError httpError model

                        ( modelGeneral, cmdGeneral ) =
                            callbacks.error httpError modelSpecific
                    in
                        ( modelGeneral, Cmd.batch [ cmdSpecific, cmdGeneral ] )
            )

        FindByExample result ->
            (case result of
                Ok relationship ->
                    callbacks.findByExample relationship model

                Err httpError ->
                    let
                        ( modelSpecific, cmdSpecific ) =
                            callbacks.findByExampleError httpError model

                        ( modelGeneral, cmdGeneral ) =
                            callbacks.error httpError modelSpecific
                    in
                        ( modelGeneral, Cmd.batch [ cmdSpecific, cmdGeneral ] )
            )

        Create result ->
            (case result of
                Ok relationship ->
                    callbacks.create relationship model

                Err httpError ->
                    let
                        ( modelSpecific, cmdSpecific ) =
                            callbacks.createError httpError model

                        ( modelGeneral, cmdGeneral ) =
                            callbacks.error httpError modelSpecific
                    in
                        ( modelGeneral, Cmd.batch [ cmdSpecific, cmdGeneral ] )
            )

        Retrieve result ->
            (case result of
                Ok relationship ->
                    callbacks.retrieve relationship model

                Err httpError ->
                    let
                        ( modelSpecific, cmdSpecific ) =
                            callbacks.retrieveError httpError model

                        ( modelGeneral, cmdGeneral ) =
                            callbacks.error httpError modelSpecific
                    in
                        ( modelGeneral, Cmd.batch [ cmdSpecific, cmdGeneral ] )
            )

        Update result ->
            (case result of
                Ok relationship ->
                    callbacks.update relationship model

                Err httpError ->
                    let
                        ( modelSpecific, cmdSpecific ) =
                            callbacks.updateError httpError model

                        ( modelGeneral, cmdGeneral ) =
                            callbacks.error httpError modelSpecific
                    in
                        ( modelGeneral, Cmd.batch [ cmdSpecific, cmdGeneral ] )
            )

        Delete result ->
            (case result of
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
            )


routes root =
    { findAll = root ++ "relationship"
    , findByExample = root ++ "relationship/example"
    , create = root ++ "relationship"
    , retrieve = root ++ "relationship/"
    , update = root ++ "relationship/"
    , delete = root ++ "relationship/"
    }


findAllTask : String -> Http.Request (List Relationship)
findAllTask root =
    Http.request
        { method = "GET"
        , headers = []
        , url = routes root |> .findAll
        , body = Http.emptyBody
        , expect = Http.expectJson (Decode.list relationshipDecoder)
        , timeout = Nothing
        , withCredentials = False
        }


findByExampleTask : String -> Relationship -> Http.Request (List Relationship)
findByExampleTask root model =
    Http.request
        { method = "POST"
        , headers = []
        , url = routes root |> .findByExample
        , body = Http.jsonBody <| relationshipEncoder model
        , expect = Http.expectJson (Decode.list relationshipDecoder)
        , timeout = Nothing
        , withCredentials = False
        }


createTask : String -> Relationship -> Http.Request Relationship
createTask root model =
    Http.request
        { method = "POST"
        , headers = []
        , url = routes root |> .create
        , body = Http.jsonBody <| relationshipEncoder model
        , expect = Http.expectJson relationshipDecoder
        , timeout = Nothing
        , withCredentials = False
        }


retrieveTask : String -> String -> Http.Request Relationship
retrieveTask root id =
    Http.request
        { method = "GET"
        , headers = []
        , url = (routes root |> .retrieve) ++ id
        , body = Http.emptyBody
        , expect = Http.expectJson relationshipDecoder
        , timeout = Nothing
        , withCredentials = False
        }


updateTask : String -> String -> Relationship -> Http.Request Relationship
updateTask root id model =
    Http.request
        { method = "PUT"
        , headers = []
        , url = (routes root |> .update) ++ id
        , body = Http.jsonBody <| relationshipEncoder model
        , expect = Http.expectJson relationshipDecoder
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
