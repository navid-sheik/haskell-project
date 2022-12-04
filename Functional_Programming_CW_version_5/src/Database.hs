{-# LANGUAGE OverloadedStrings #-}
-- or, on GHCI:
-- > :set -XOverloadedStrings
module Database (
    initialiseDB,
    getOrCreateProgram,
    createRecord,
    saveRecordsEntries,
    saveRecordsSummary,
    createSummary,
    getRandomInt,
    queryProgramAllEntries,
    queryProgramSummary,
    queryAllProgramEnded,
    queryProgramPremieredDate,
    query5RandomPrograms,
    queryProgramRandomSummary,
    countAllRunningProgram,
    queryRunningProgram
) where

import Types
import Database.SQLite.Simple
import System.Random
-- See more Database.SQLite.Simple examples at
-- https://hackage.haskell.org/package/sqlite-simple-0.4.18.0/docs/Database-SQLite-Simple.html


-- Database initialisation and helpers functions
initialiseDB :: IO Connection
initialiseDB = do
        conn <- open "program.sqlite"
        execute_ conn "CREATE TABLE IF NOT EXISTS program (\
        \id INTEGER PRIMARY KEY AUTOINCREMENT,\
        \program VARCHAR(200) NOT NULL\
        \)"
        execute_ conn "CREATE TABLE IF NOT EXISTS entries (\
        \weight INT,\
        \runtime INT,\
        \premiered VARCHAR(40), \
        \ended INT DEFAULT NULL,\
        \language INT DEFAULT NULL, \
        \site VARCHAR(300),\
        \status VARCHAR(80) DEFAULT NULL,\
        \fk_program INTEGER\
        \)"
        execute_ conn "CREATE TABLE IF NOT EXISTS summary (\
        \summary VARCHAR(2000) NOT NULL,\
        \fk_program INTEGER\
        \)"
        return conn

-- Insert the date into the database
getOrCreateProgram :: Connection -> String -> IO Program
getOrCreateProgram conn prog = do
    results <- queryNamed conn "SELECT * FROM program WHERE program=:program" [":program" := prog]    
    if length results > 0 then
        return . head $ results
    else do
        execute conn "INSERT INTO program (program) VALUES (?)" [prog :: String]
        getOrCreateProgram conn prog


-- Create a record and insert the value in entry table
createRecord :: Connection -> Record -> IO ()
createRecord conn record = do
    c <- getOrCreateProgram conn (program record)
    let entry = Entry {
        weight_ = weight record :: Maybe Int,
        runtime_ = runtime record :: Maybe Int,
        premiered_ = premiered record :: Maybe String,
        ended_ = ended record :: Maybe String,
        language_ = language record :: Maybe String,
        site_ = site record :: Maybe String,
        status_ = status record :: Maybe String,
        fk_program1 = id_ c :: Maybe Int
    }
    execute conn "INSERT INTO entries VALUES (?,?,?,?,?,?,?,?)" entry

-- Insert the data inside the summary table 
createSummary :: Connection -> Record -> IO ()
createSummary conn record  = do
    c <- getOrCreateProgram conn (program record)
    let insertsummary = Summary {
        summary_ = summary record :: Maybe String,
        fk_program2 = id_ c :: Maybe Int
    }
    execute conn "INSERT INTO summary VALUES (?,?)" insertsummary


-- Convert the result in a list in a single integer
listToInt :: [Int] -> Int
listToInt [] = 0
listToInt (x:xs) = x


-- Generate random number between 1, 230 (the number of records in our database)
getRandomInt :: IO Int
getRandomInt = do
   g <- getStdGen
   let num = listToInt (take 1 (randomRs (1, 230) g :: [Int]))
   return num 

-- Invoke function to create entries records
saveRecordsEntries :: Connection -> [Record] -> IO ()
saveRecordsEntries conn = mapM_ (createRecord conn)
-- Invoke function to create summary records
saveRecordsSummary :: Connection -> [Record] -> IO ()
saveRecordsSummary conn = mapM_ (createSummary conn)


-- QUERIES 
-- Display certain number of shows
queryProgramAllEntries :: Connection -> IO [Program]
queryProgramAllEntries conn = do
    putStrLn "-----------------------------------------"
    putStr "Enter how many shows you want to display  > "
    numberOfPrograms <- getLine
    let sql = "SELECT  * FROM program  LIMIT ? "
    query conn sql [numberOfPrograms]

-- Display the 4 random options + the solution record  to the game 
query5RandomPrograms :: Connection -> Int -> IO [Program]
query5RandomPrograms conn n = do
    putStrLn "-----------------------------------------"
    putStrLn "Pick amongst these programs : "
    let sql = "SELECT  * FROM program  WHERE program.id = ?  UNION  SELECT * FROM (SELECT  * FROM program  ORDER BY RANDOM() LIMIT 4)  ORDER BY id"
    query conn sql [n]

-- Display the random summary and it's status
queryProgramRandomSummary :: Connection -> Int ->IO [RandomSummary]
queryProgramRandomSummary  conn  randomID= do
    putStrLn "-----------------------------------------"
    putStrLn "Guess ID of this movie's summary : "
    let sql = "SELECT DISTINCT REPLACE(REPLACE(REPLACE(REPLACE(summary, '<p>', ''),'</p>', ''),'<b>', '') ,'</b>', '') , entries.status FROM summary, entries where summary.fk_program = ? AND entries.fk_program = ?"
    query conn sql [randomID, randomID]

-- Show the summary of a particular show
queryProgramSummary :: Connection -> IO [Summary]
queryProgramSummary  conn = do
    putStrLn "-----------------------------------------"
    putStr "Enter a program ID to display the summary > "
    id_show <- getLine
    let sql = "SELECT DISTINCT REPLACE(REPLACE(REPLACE(REPLACE(summary, '<p>', ''),'</p>', ''),'<b>', '') ,'</b>', ''), fk_program FROM summary where summary.fk_program = ?"
    query conn sql [id_show]

--Show all shows that ended 
queryAllProgramEnded :: Connection ->IO[ShowWithDate]
queryAllProgramEnded conn = do
    let sql  = "SELECT program.id, program, ended FROM program, entries WHERE entries.fk_program == program.id  AND entries.status='Ended' "
    query_ conn sql 

-- Show all the programs premiered before a certain year
queryProgramPremieredDate ::  Connection -> IO[ShowWithDate]
queryProgramPremieredDate conn = do
    putStrLn "-----------------------------------------"
    putStr "Filter movie by year - Enter premier year  > "
    premieredYear <- getLine
    let sql  = "SELECT  program.id, program, premiered FROM entries, program WHERE entries.fk_program == program.id AND entries.premiered <  ? ORDER BY entries.premiered ASC "
    query conn sql [premieredYear ++ "-01-01" ] 

--Helper query - query all programs that are running
queryRunningProgram :: Connection ->IO[ShowWithDate]
queryRunningProgram conn = do
    let sql  = "SELECT program.id, program, ended FROM program, entries WHERE entries.fk_program == program.id  AND entries.status='Running' "
    query_ conn sql

-- With help of the queryRunningProgram, count the numbers of records 
countAllRunningProgram :: Connection -> IO ()
countAllRunningProgram conn = do
    runningPrograms <- queryRunningProgram conn
    let total = length runningPrograms
    print $ "The number of running programs in 2022 is : " ++ show(total)