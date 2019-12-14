module Commands where

import Prelude hiding ((<>))
import qualified Data.Yaml as Yaml (encode, decode)
import qualified Data.ByteString.Char8 as BS (readFile, writeFile)
import System.IO.Error (isDoesNotExistError)
import Control.Exception (catchJust)
import Control.Monad (forM_)
import System.Directory (doesFileExist)
import Types
import Model
import qualified Data.Map as M (empty, assocs)
import Data.Time (TimeOfDay)

showInfo :: FilePath -> IO()
showInfo dataPath = do
    putStrLn $ "Data file path: " ++ dataPath
    exists <- doesFileExist dataPath
    if exists
    then do
        s <- BS.readFile dataPath
        let mbToDoList = Yaml.decode s
        case mbToDoList of
            Nothing -> putStrLn $ "Status: file is invalid"
            Just (Timesheet timesheet) -> putStrLn $ "Status: contains " ++ show (length timesheet) ++ " year(s)"
    else putStrLn $ "Status: file does not exist"

initTimesheet :: FilePath -> IO()
initTimesheet dataPath = writeTimesheet dataPath (Timesheet M.empty)

showMonth :: FilePath -> YearNumber -> MonthNumber -> IO()
showMonth dataPath yearNumber monthNumber = do
    timesheet <- readTimesheet dataPath
    
    let mbMonth = monthLookup'' yearNumber monthNumber timesheet

    case mbMonth of
        Nothing -> putStrLn "Status: month not found"
        Just month -> do
                        putStrLn $ "[" ++ (show yearNumber) ++ "/" ++ (show monthNumber) ++ "]"
                        forM_
                            (M.assocs $ days month)
                            (\(n,d) -> showDay n d)
        
showDay :: DayNumber -> Day -> IO ()
showDay dayNumber day = do
    putStrLn $ "Day: " ++ show dayNumber
    putStrLn $ show day
    let (worked, left, balance) = calculateBalance day
    putStrLn $ "Worked: " ++ show worked
    putStrLn $ "Left: " ++ show left
    putStrLn $ "Balance: " ++ show balance
    putStrLn $ "--------------------"

punch :: FilePath -> PunchDate -> IO()
punch dataPath punch = do
    timesheet <- readTimesheet dataPath
    let newTimesheet = addPunch punch timesheet
    writeTimesheet dataPath newTimesheet

deletePunch :: FilePath -> PunchDate -> IO()
deletePunch dataPath punch = do
    timesheet <- readTimesheet dataPath
    let newTimesheet = removePunch punch timesheet
    writeTimesheet dataPath newTimesheet

-- removeItem :: FilePath -> ItemIndex -> IO ()
-- removeItem dataPath idx = do
--     ToDoList items <- readToDoList dataPath
--     let mbItems = removeAt idx items
--     case mbItems of
--         Nothing -> putStrLn "Invalid item index"
--         Just items' -> writeToDoList dataPath (ToDoList items')
--     where
--         removeAt :: Int -> [a] -> Maybe [a]
--         removeAt n xs
--             | n >= 0 && n < length xs = Just $ take n xs ++ drop (n+1) xs
--             | otherwise = Nothing

writeTimesheet :: FilePath -> Timesheet -> IO ()
writeTimesheet dataPath timesheet = BS.writeFile dataPath (Yaml.encode timesheet)

readTimesheet :: FilePath -> IO Timesheet
readTimesheet dataPath = do
    mbTimesheet <- catchJust
        (\e -> if isDoesNotExistError e then Just () else Nothing)
        (BS.readFile dataPath >>= return . Yaml.decode)
        (\_ -> return $ Just (Timesheet M.empty))
    case mbTimesheet of
        Nothing -> error "YAML file is corrupt"
        Just timesheet -> return timesheet
