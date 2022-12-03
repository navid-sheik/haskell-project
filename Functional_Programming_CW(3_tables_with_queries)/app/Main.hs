module Main (main) where

import System.IO
import Types
import Fetch
import Parse
import Database

main :: IO ()
main = do
    putStrLn "---------------------------------"
    putStrLn "  Welcome to the Show data app   "
    putStrLn "  (1) Download data              "
    putStrLn "  (2) Show number of of shows    "
    putStrLn "  (3) Show Summary based on id    "
    putStrLn "  (4) Count the number of running shows       "
    putStrLn "  (5) Premier date       "
    putStrLn "  (6) Random       "
    putStrLn "  (7) Quit                       "
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
            entries <- queryProgramAllEntries conn
            mapM_ print entries
            main


        3 -> do
            entries <- queryProgramSummary conn
            mapM_ print entries
            main

        -- 4 -> do 
        --     queryTotalNumberOfProgramEnded  conn
        --     main
        4 -> do
            entries <- queryAllProgramEnded conn
            mapM_ print entries
            main

        5 -> do
            entries <- queryProgramPremieredDate conn
            mapM_ print entries
            main
        6 -> do
            answerNum <- getRandomInt
            putStrLn "Welcome to guess show"
            randomSummary <- queryProgramRandomSummary conn answerNum
            mapM_ print randomSummary
            putStrLn "-----------------------------------------"
            entries <- query5RandomPrograms conn answerNum
            mapM_ print entries
            putStrLn "-----------------------------------------"
            putStr "Choose an option > "
            choice <- readLn :: IO Int
            if choice == answerNum
                then do 
                putStrLn "Correct!"
                main 
               else do 
                putStrLn "Wrong, better luck next time!"
                main
      
            -- putStrLn "Is this show 1"
            -- putStrLn "or 2"
            -- answerNum <- getRandomInt
            -- putStrLn "BEFORE"
            -- answerProg <- queryProgram conn
            -- putStrLn "AFTER"
            -- mapM_ print answerProg
            -- putStr "Choose an option > "
            -- choice <- readLn :: IO Int
            -- if choice == answerNum 
            --    then do 
            --     putStrLn "Correct!"
            --     main 
            --    else do 
            --     putStrLn "Wrong, better luck next time!"
            --     main
            -- case choice of
            --     answer -> do
            --         print answer
            --         putStrLn "Correct!"
            --         main
            --     2 -> do
            --         putStrLn "Wrong!"
            --         main
            --     _ -> do
            --         print "Invalid option"
            --         main
        7 -> print "Hope you've enjoyed using the app!"
        _ -> print "Invalid option"
