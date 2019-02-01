module Main

import Data.Vect

data Schema: Type where
    SString: Schema
    SInt: Schema 
    SChar: Schema 
    Splus: Schema -> Schema -> Schema

SchemaType: Schema -> Type
SchemaType SString = String
SchemaType SInt = Integer
SchemaType SChar = Char
SchemaType (Splus x y) = (SchemaType x, SchemaType y)

record DataStore where
    constructor MkDataStore
    schema: Schema
    size : Nat
    items : Vect size (SchemaType schema)


addToStore : (ds:DataStore) -> (SchemaType (schema ds)) -> DataStore
addToStore (MkDataStore schema size store) newitem = MkDataStore _ _ (addToData store) where
    addToData : Vect oldsize (SchemaType schema) -> Vect (S oldsize) (SchemaType schema) 
    addToData [] = [newitem]
    addToData (x :: xs) = x :: addToData xs

data Command : Schema -> Type where
    SetSchema: Schema -> Command s
    GetSchema: Command s
    Add: SchemaType s -> Command s
    Get: Integer -> Command s
    GetAll: Command s
    Size: Command s
    Search: String -> Command s
    Quit: Command s

parseSchemaDef: String -> Maybe Schema
parseSchemaDef x = case span (/= ' ') (trim x) of 
    ("String", "") => Just SString
    ("Int", "") => Just SInt
    ("Char", "") => Just SChar
    (_, "") => Nothing
    (s, rest) => do x <- (parseSchemaDef s)
                    y <- (parseSchemaDef rest)
                    Just (Splus x y)
    _ => Nothing

readInt: String -> Maybe Integer
readInt val = case all isDigit (unpack val) of
    False => Nothing
    True => Just (cast val)

parseSchema : (s: Schema) -> String -> Maybe (SchemaType s)
parseSchema SString x = Just x
parseSchema SInt i = readInt i
parseSchema SChar x = if (length x == 1) then Just (strHead x) else Nothing
parseSchema (Splus y z) x = case span (/= ' ') (trim x) of
    (xy, "") => Nothing
    (xy, xz) => do psy <- parseSchema y (trim xy)
                   psz <- parseSchema z (trim xz)
                   Just (psy, psz)

parseCommand : (schema: Schema) -> String -> String -> Maybe (Command schema)
parseCommand schema "schema" str = (parseSchemaDef str) >>= (pure . SetSchema)
parseCommand schema "getschema" "" = Just GetSchema
parseCommand schema "add" str = map Add (parseSchema schema str)
parseCommand schema "get" "" = Just GetAll
parseCommand schema "get" val = map Get $ readInt val
parseCommand schema "size" "" = Just Size
parseCommand schema "search" term = Just (Search term)
parseCommand schema "quit" "" = Just Quit
parseCommand _ _ _ = Nothing

parse : (schema: Schema) -> (input : String) -> Maybe (Command schema)
parse schema input = case span (/= ' ') input of
    (cmd, args) => parseCommand schema cmd (ltrim args)

display : SchemaType s -> String
display {s = SString} str = str
display {s = SInt} i = cast i
display {s = SChar} c = cast c
display {s = (Splus y z)} x = case x of (x1, x2) => display x1 ++ ", " ++ display x2

showSchema: Schema -> String
showSchema SString = "String"
showSchema SInt = "Int"
showSchema SChar = "Char"
showSchema (Splus x y) = "(" ++ showSchema x ++ ", " ++ showSchema y ++ ")"

getEntry : (pos : Integer) -> (store : DataStore) -> Maybe (String, DataStore)
getEntry pos store = let store_items = items store in
    case integerToFin pos (size store) of
        Nothing => Just ("Out of range\n", store)
        Just id => Just ((display (index id (items store))) ++ "\n", store)

matches : (term : String) -> (y : SchemaType s) -> Bool
matches {s = SString} term str = isInfixOf term str
matches {s = SInt} term i = i == (cast term)
matches {s = SChar} term c = term == (cast c)
matches {s = (Splus x z)} term y = matches term (fst y) || matches term (snd y)


searchItems: (s: Schema) -> Nat -> String -> Vect n (SchemaType s) -> String
searchItems _ _ _  [] = ""
searchItems s k term (y :: xs) = let rest = searchItems s (S k) term xs in
    if matches term y 
    then ((show k) ++ ": " ++ (display y) ++ "\n" ++ rest)
    else rest

search : String -> DataStore -> String
search term store = searchItems (schema store) 0 term (items store)

zipWithIndexFrom : Nat -> Vect n a -> Vect n (Nat, a)
zipWithIndexFrom x [] = []
zipWithIndexFrom x (y :: xs) = (x, y) :: zipWithIndexFrom (S x) xs

zipWithIndex: Vect n a -> Vect n (Nat, a)
zipWithIndex xs = zipWithIndexFrom 0 xs


getAll : DataStore -> String
getAll ds = unwords . toList $ (map (\i => (show (fst i)) ++ ": " ++ display (snd i) ++ "\n") (zipWithIndex (items ds)))

processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store input = case parse (schema store) input of
    Nothing => Just ("Invalid command\n", store)
    Just (Add item) => Just ("ID " ++ show (size store) ++ "\n", addToStore store item)
    Just GetAll => Just (getAll store, store)
    Just (Get pos) => getEntry pos store
    Just Size => Just (show (size store) ++ "\n", store)
    Just (Search term) => Just (search term store, store)
    Just Quit => Nothing
    Just (SetSchema s) => Just ("OK\n", MkDataStore s 0 [])
    Just GetSchema => Just (showSchema (schema store) ++ "\n", store)

main : IO ()
main = replWith (MkDataStore SString _ []) "Command: " processInput
