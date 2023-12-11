import qualified Data.Map as Map

newtype Storage = Storage { getStorage :: Map.Map String Float }

emptyStorage :: Storage
emptyStorage = Storage Map.empty

insertValue :: String -> Float -> Storage -> Storage
insertValue key value (Storage storage) = Storage $ Map.insert key value storage

readValue :: String -> Storage -> Maybe Float
readValue key (Storage storage) = Map.lookup key storage

storage2Str :: Storage -> String
storage2Str (Storage storage) = Map.foldrWithKey (\key value acc -> key ++ "=" ++ show value ++ "," ++ acc) "" storage


-- Documentation:

-- Creating an empty Storage
-- myStorage = emptyStorage

-- Inserting values
-- updatedStorage = insertValue "key1" 3.14 myStorage -- This will return a new Storage with the value inserted

-- Reading values
-- valueForKey1 = readValue "key1" updatedStorage  -- This will return the "key1" value or Nothing is "key1" doesn't exist