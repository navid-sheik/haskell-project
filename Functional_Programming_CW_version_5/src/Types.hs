{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE DeriveAnyClass, TypeFamilies #-}


module Types (
    Entry (..),
    Program (..),
    Summary (..),
    Record (..),
    ShowWithDate(..),
    RandomSummary (..)
) where

import GHC.Generics
import Data.Aeson
import Data.Aeson.Types

import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.ToRow

--CUSTOM DATA TYPES
-- Used to create rows in the entries table and display info
data Entry = Entry {
    weight_ :: Maybe Int,
    runtime_ :: Maybe Int,
    premiered_ :: Maybe String,
    ended_ :: Maybe String,
    language_ :: Maybe String,
    site_ :: Maybe String,
    status_ :: Maybe String,
    fk_program1 :: Maybe Int
} deriving (Show)

-- Used to store programs in program table and display the ID, title
data Program = Program {
    id_ :: Maybe Int,
    program_ :: String
} deriving (Show)

-- Used for displaying a summary and it's ID 
data Summary = Summary {
    summary_ :: Maybe String,
    fk_program2 :: Maybe Int
} deriving (Show)

-- Used for displaying random summary 
data RandomSummary = RandomSummary {
    guess_summary :: Maybe String,
    guess_status :: Maybe String
} deriving (Show)

-- Used for  displaying the ID, title and date of a show
data ShowWithDate = ShowWithDate {
    show_id :: Maybe Int,
    show_title :: String,
    show_date :: Maybe String
} deriving (Show)

-- Used to covert JSON data and store record in datase
data Record = Record {
    weight :: Maybe Int,
    runtime :: Maybe Int,
    premiered :: Maybe String,
    ended :: Maybe String,
    language :: Maybe String,
    site :: Maybe String,
    status :: Maybe String,
    program :: String,
    summary :: Maybe String
} deriving (Show, Generic)


-- Constructors for rows 
instance FromRow Record where
    fromRow = Record <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

instance FromRow Program where
    fromRow = Program <$> field <*> field 

instance FromRow ShowWithDate where
    fromRow = ShowWithDate <$> field <*> field <*> field 

instance FromRow Summary where
    fromRow = Summary <$> field <*> field

instance FromRow RandomSummary where
    fromRow = RandomSummary <$> field <*> field

instance FromRow Entry where
    fromRow = Entry <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field


-- Constructors for rows 
instance ToRow Entry where
    toRow (Entry w rt p e l si st fk_p1)
        = toRow (w, rt, p, e, l, si, st, fk_p1)

instance ToRow Summary where
    toRow (Summary s fk_p2)
        = toRow (s, fk_p2)

instance ToRow RandomSummary where
    toRow (RandomSummary r_s r_st)
        = toRow (r_s, r_st)

instance ToRow Program where
    toRow (Program i prog)
        = toRow (i, prog)

instance ToRow ShowWithDate where
    toRow (ShowWithDate s_i s_t s_d)
        = toRow (s_i, s_t, s_d)

-- Rename JSONN fields
renameFields :: String -> String
renameFields "program" = "name"
renameFields "site" = "officialSite"
renameFields other = other

-- Apply renaming fields and allow null value to be accepted in json
customOptions :: Options
customOptions = defaultOptions {
    fieldLabelModifier = renameFields,
    omitNothingFields = True
}

-- Parse JSON data
instance FromJSON Record where
    parseJSON = genericParseJSON customOptions