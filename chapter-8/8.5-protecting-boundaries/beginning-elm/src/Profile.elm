module Profile exposing (Name, createName)


type Name
    = Name String String


createName : String -> String -> Name
createName firstName lastName =
    Name firstName lastName
