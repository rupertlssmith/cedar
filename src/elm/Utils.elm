module Utils exposing (error, message, Update, lift)

import Dict exposing (Dict)
import Set exposing (Set)
import Maybe
import Maybe.Extra exposing (isJust)
import List
import Http
import Task
import Auth


{-
   Combines a list of unary tests on some model into a single unary test on the
   model with the result being the conjunction all of the individual tests.
-}


checkAll : List (model -> Bool) -> model -> Bool
checkAll checks model =
    List.map (\check -> check model) checks |> List.foldl (&&) True



{-
   A defalt HTTP error handler that maps:
   401 UNAUTHED -> Auth.unauthed
-}


error : Http.Error -> model -> ( model, Cmd msg )
error httpError model =
    case httpError of
        Http.BadStatus response ->
            if (response.status.code == 401) then
                ( model, Auth.unauthed )
            else if (response.status.code == 403) then
                ( model, Auth.unauthed )
            else
                ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )



{-
   Finds the nth element of a list.
-}


nth : Int -> List a -> Maybe a
nth k xs =
    List.drop k xs |> List.head



{-
   Computes the symetric difference of two dictionaries. The result contains items
   appearing only in one or other of the inputs.
-}


symDiff : Dict comparable a -> Dict comparable a -> Dict comparable a
symDiff dict1 dict2 =
    let
        insertNeither _ _ _ dict =
            dict
    in
        Dict.merge Dict.insert insertNeither Dict.insert dict1 dict2 Dict.empty



{-
   Computes the key intersection of two dictionaries, keeping the values from the left.
-}


leftIntersect : Dict comparable a -> Dict comparable b -> Dict comparable a
leftIntersect dict1 dict2 =
    let
        insertLeft key leftVal rightVal dict =
            Dict.insert key leftVal dict

        ignore _ _ dict =
            dict
    in
        Dict.merge ignore insertLeft ignore dict1 dict2 Dict.empty



{-
   Tranforms a list of entities (records with a String id), into a Dict, with the ids
   as keys.

   Any entities with missing ids are not included in the output.
-}


dictifyEntities : (b -> { a | id : Maybe String }) -> ({ a | id : Maybe String } -> b) -> List b -> Dict String b
dictifyEntities unwrapper wrapper entities =
    Dict.fromList <|
        List.map (\rec -> ( Maybe.withDefault "" rec.id, wrapper rec )) <|
            List.filter (\rec -> isJust rec.id) <|
                List.map unwrapper entities



{-
   Extracts the key set from a dict.
-}


keySet : Dict comparable v -> Set comparable
keySet dict =
    Dict.keys dict |> Set.fromList



{-
   Performs a right fold on a dictionary, supplying item indexs as the dictionary is iterated.
-}


indexedFoldr : (number -> comparable -> v -> b -> b) -> b -> Dict comparable v -> b
indexedFoldr fun acc list =
    let
        ( highest, result ) =
            Dict.foldr (\key -> \item -> \( idx, items ) -> ( idx + 1, fun idx key item items )) ( 0, acc ) list
    in
        result



{-
   Performs a left fold on a dictionary, supplying item indexs as the dictionary is iterated.
-}


indexedFoldl : (number -> comparable -> v -> b -> b) -> b -> Dict comparable v -> b
indexedFoldl fun acc list =
    let
        ( highest, result ) =
            Dict.foldl (\key -> \item -> \( idx, items ) -> ( idx + 1, fun idx key item items )) ( 0, acc ) list
    in
        result



{-
   Cleans string input to a maybe.
-}


cleanString : String -> Maybe String
cleanString val =
    if "" == val then
        Nothing
    else
        Just val



{-
   Converts a maybe String to "" in the case that it is Nothing.
-}


valOrEmpty : Maybe String -> String
valOrEmpty maybeVal =
    case maybeVal of
        Nothing ->
            ""

        Just val ->
            val



{-
   Toggles a value in a set. If the value is present, it is removed, if it is
   not present it is inserted.
-}


toggleSet : comparable -> Set comparable -> Set comparable
toggleSet key set =
    if Set.member key set then
        Set.remove key set
    else
        Set.insert key set


{-| A command to generate a message without performing any action.
This is useful for implementing components that generate events in the manner
of HTML elements, but where the event fires from within Elm code, rather than
by an external trigger.
-}
message : msg -> Cmd msg
message x =
    Task.perform identity (Task.succeed x)


{-| Variant of EA update function type, where effects may be
lifted to a different type.
-}
type alias Update_ model action action_ =
    action -> model -> ( model, Cmd action_ )


{-| Standard EA update function type.
-}
type alias Update model action =
    Update_ model action action


{-| Convenience function for writing update-function boilerplate. Example use:
    case msg of
      ...
      ButtonsMsg msg_ ->
        lift .buttons (\m x->{m|buttons=x}) ButtonsMsg Demo.Buttons.update msg_ model
This is equivalent to the more verbose
    case msg of
      ...
      ButtonsMsg msg_ ->
        let
          (buttons_, cmd) =
            Demo.Buttons.update msg_ model.buttons
        in
          ( { model | buttons = buttons_}
          , Cmd.map ButtonsMsg cmd
          )
-}
lift :
    (model -> submodel)
    -> (model -> submodel -> model)
    -> (subaction -> action)
    -> Update submodel subaction
    -> subaction
    -> model
    -> ( model, Cmd action )
lift get set fwd update action model =
    let
        ( submodel_, e ) =
            update action (get model)
    in
        ( set model submodel_, Cmd.map fwd e )
