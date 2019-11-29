module Main where

    import Parsers
    import Commands
    import Types
    import Options.Applicative (execParser, info, progDesc)
    import Data.String.Utils (replace)
    import System.Directory (getHomeDirectory)
    
    main :: IO ()
    main = do
        Options dataPath command <- execParser $ info optionsParser (progDesc "Timesheet manager")
        homeDir <- getHomeDirectory
        let expandedDataPath = replace "~" homeDir dataPath
        run expandedDataPath command
    
    run :: FilePath -> Command -> IO ()
    run dataPath Info = showInfo dataPath
    run dataPath Init = initItems dataPath
    run dataPath List = viewItems dataPath
    run dataPath (Add item) = addItem dataPath item
    run dataPath (View idx) = viewItem dataPath idx
    run dataPath (Update idx itemUpdate) = putStrLn $ "Update: idx=" ++ show idx ++ " itemUpdate=" ++ show itemUpdate
    run dataPath (Remove idx) = removeItem dataPath idx
    