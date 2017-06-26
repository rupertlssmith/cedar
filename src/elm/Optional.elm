module Optional exposing (when, required, optional)


when : Bool -> a -> Maybe a
when condition value =
    if (condition) then
        Just value
    else
        Nothing


required : a -> Maybe a
required value =
    Just value


optional : List (Maybe a) -> List a
optional =
    List.filterMap identity
