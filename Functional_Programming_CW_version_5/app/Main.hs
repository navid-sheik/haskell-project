module Main (main) where

import System.IO
import Types
import Fetch
import Parse
import Database

main :: IO ()
main = do
    putStrLn "---------------------------------"
    putStrLn "  Welcome to TV SERIES program"
    putStrLn "  (1) Download data"
    putStrLn "  (2) Display shows"
    putStrLn "  (3) Show summary of a show (ID) "
    putStrLn "  (4) Show all programs that ENDED"
    putStrLn "  (5) Show all  movies premiered before YEAR"
    putStrLn "  (6) Guess the shows - GAME"
    putStrLn "  (7) Count the number of RUNNING shows"
    putStrLn "  (8) Quit                       "
    putStrLn "---------------------------------"
    conn <- initialiseDB
    hSetBuffering stdout NoBuffering
    putStr "Choose an option > "
    option <- readLn :: IO Int
    case option of
        1 -> do
            let url = "https://api.tvmaze.com/shows"
            print "Downloading..."
            json <- download url
            print "Parsing..."
            case (parseRecords json) of
                Left err -> print err
                Right recs -> do
                    print "Saving on DB..."
                    saveRecordsEntries conn (recs)
                    saveRecordsSummary conn (recs)
                    print "Saved!"
                    main

        2 -> do
            -- Display number of shows 
            entries <- queryProgramAllEntries conn
            mapM_ print entries
            main
        3 -> do
            -- Display summary based on ID 
            entries <- queryProgramSummary conn
            mapM_ print entries
            main
        4 -> do
            -- Show all the programs ended 
            entries <- queryAllProgramEnded conn
            mapM_ print entries
            main

        5 -> do
            -- Filter by premiered date
            entries <- queryProgramPremieredDate conn
            mapM_ print entries
            main
        6 -> do
            -- MINI GAME 
            -- Generate random ID (1,230)
            answerNum <- getRandomInt
            putStrLn "Welcome to GUESS  show"
            -- Get the summary and the status with random ID generated
            randomSummary <- queryProgramRandomSummary conn answerNum
            mapM_ print randomSummary
            -- Create 5 options to guess from 
            entries <- query5RandomPrograms conn answerNum
            mapM_ print entries
            putStrLn "-----------------------------------------"
            putStr "Guess the ID show based on the description above > "
            -- Get the choice from the user 
            choice <- readLn :: IO Int
            -- Display the result
            if choice == answerNum
                then do 
                putStrLn "Correct!"
                main 
               else do 
                putStrLn  $ "The answer was " ++ (show(answerNum))
                putStrLn "Wrong, better luck next time!"
                main
        7 -> do
            -- Count the number of running shows 
            countAllRunningProgram conn
            main
        8 -> print "Hope you've enjoyed using the app!"
        _ -> print "Invalid option"
