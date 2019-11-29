module Commands where

import qualified Data.Yaml as Yaml (encode, decode)
import qualified Data.ByteString.Char8 as BS (readFile, writeFile)
import System.IO.Error (isDoesNotExistError)
import Control.Exception (catchJust)
import Control.Monad (forM_)
import Data.Time (formatTime, defaultTimeLocale)
import System.Directory (doesFileExist)
import Data.List.Safe ((!!))
import Prelude hiding ((!!))
import Types

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
            Just (ToDoList items) -> putStrLn $ "Status: contains " ++ show (length items) ++ " item(s)"
    else putStrLn $ "Status: file does not exist"

initItems :: FilePath -> IO()
initItems dataPath = writeToDoList dataPath (ToDoList [])

viewItems :: FilePath -> IO()
viewItems dataPath = do
    ToDoList items <- readToDoList dataPath
    forM_
        (zip [0..] items)
        (\(idx, item) -> showItem idx item)

addItem :: FilePath -> Item -> IO()
addItem dataPath item = do
    ToDoList items <- readToDoList dataPath
    let newToDoList = ToDoList (items ++ [item])
    writeToDoList dataPath newToDoList

viewItem :: FilePath -> ItemIndex -> IO ()
viewItem dataPath idx = do
    ToDoList items <- readToDoList dataPath
    let mbItem = items !! idx
    case mbItem of
        Nothing -> putStrLn "Invalid item index"
        Just item -> showItem idx item

showItem :: ItemIndex -> Item -> IO ()
showItem idx (Item title mbDescription mbPriority mbDueBy) = do
    putStrLn $ "[" ++ show idx ++ "]: " ++ title
    putStr " Description: "
    putStrLn $ showField id mbDescription
    putStr " Priority: "
    putStrLn $ showField show mbPriority
    putStr " Due by: "
    putStrLn $ showField (formatTime defaultTimeLocale "%Y/%m/%d %H:%M:%S") mbDueBy

showField :: (a -> String) -> Maybe a -> String
showField f (Just x) = f x
showField _ Nothing = "(not set)"

removeItem :: FilePath -> ItemIndex -> IO ()
removeItem dataPath idx = do
    ToDoList items <- readToDoList dataPath
    let mbItems = removeAt idx items
    case mbItems of
        Nothing -> putStrLn "Invalid item index"
        Just items' -> writeToDoList dataPath (ToDoList items')
    where 
        removeAt :: Int -> [a] -> Maybe [a]
        removeAt n xs 
            | n >= 0 && n < length xs = Just $ take n xs ++ drop (n+1) xs
            | otherwise = Nothing
            
writeToDoList :: FilePath -> ToDoList -> IO ()
writeToDoList dataPath toDoList = BS.writeFile dataPath (Yaml.encode toDoList)

readToDoList :: FilePath -> IO ToDoList
readToDoList dataPath = do
    mbToDoList <- catchJust
        (\e -> if isDoesNotExistError e then Just () else Nothing)
        (BS.readFile dataPath >>= return . Yaml.decode)
        (\_ -> return $ Just (ToDoList []))
    case mbToDoList of 
        Nothing -> error "YAML file is corrupt"
        Just toDoList -> return toDoList