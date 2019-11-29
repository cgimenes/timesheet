module Parsers where

import Data.Time (LocalTime)
import Options.Applicative hiding (infoParser)
import Types
import Data.Time (LocalTime, defaultTimeLocale, parseTimeM)

infoParser :: Parser Command
infoParser = pure Info

initParser :: Parser Command
initParser = pure Init

listParser :: Parser Command
listParser = pure List

addParser :: Parser Command
addParser = Add <$> addItemParser

addItemParser :: Parser Item
addItemParser = Item
    <$> argument str (metavar "TITLE" <> help "title")
    <*> optional itemDescriptionValueParser
    <*> optional itemPriorityValueParser
    <*> optional itemDueByValueParser

viewParser :: Parser Command
viewParser = View <$> itemIndexParser

updateParser :: Parser Command
updateParser = Update <$> itemIndexParser <*> updateItemParser

updateItemParser :: Parser ItemUpdate
updateItemParser = ItemUpdate
    <$> optional updateItemTitleParser
    <*> optional updateItemDescriptionParser
    <*> optional updateItemPriorityParser
    <*> optional updateItemDueByParser

updateItemTitleParser :: Parser ItemTitle
updateItemTitleParser = itemTitleValueParser

updateItemDescriptionParser :: Parser ItemDescription
updateItemDescriptionParser = 
    Just <$> itemDescriptionValueParser
    <|> flag' Nothing (long "clear-desc")

updateItemPriorityParser :: Parser ItemPriority
updateItemPriorityParser = 
    Just <$> itemPriorityValueParser
    <|> flag' Nothing (long "clear-priority")    

updateItemDueByParser :: Parser ItemDueBy
updateItemDueByParser = 
    Just <$> itemDueByValueParser
    <|> flag' Nothing (long "clear-due-by")    

removeParser :: Parser Command
removeParser = Remove <$> itemIndexParser

commandParser:: Parser Command
commandParser = subparser $ mconcat
    [ command "info" (info infoParser (progDesc "Show info"))
    , command "init" (info initParser (progDesc "Initialize items"))
    , command "list" (info listParser (progDesc "List all items"))
    , command "add" (info addParser (progDesc "Add item"))
    , command "view" (info viewParser (progDesc "View item"))
    , command "update" (info updateParser (progDesc "Update item"))
    , command "remove" (info removeParser (progDesc "Remove item"))
    ]

optionsParser :: Parser Options
optionsParser = Options
    <$> dataPathParser
    <*> commandParser

dataPathParser :: Parser FilePath
dataPathParser = strOption $
    value defaultDataPath
    <> long "data-path"
    <> short 'p'
    <> metavar "DATAPATH"
    <> help ("path to data file (default " ++ defaultDataPath ++ ")")
    where 
        defaultDataPath = "~/.to-do.yaml"

itemIndexParser :: Parser ItemIndex
itemIndexParser = argument auto $
    metavar "ITEMINDEX" 
    <> help "index of item"

itemTitleValueParser :: Parser String
itemTitleValueParser = strOption $
    long "title"
    <> short 't'
    <> metavar "TITLE"
    <> help "title"

itemDescriptionValueParser :: Parser String
itemDescriptionValueParser = strOption $
    long "desc"
    <> short 'd'
    <> metavar "DESCRIPTION"
    <> help "description"

itemPriorityValueParser :: Parser Priority
itemPriorityValueParser = option (eitherReader readPriority) $
    long "priority"
    <> short 'p'
    <> metavar "PRIORITY"
    <> help "priority (1: Low, 2: Normal, 3: High)"
    where 
        readPriority arg = case arg of
            "1" -> Right Low
            "2" -> Right Normal
            "3" -> Right High
            _ -> Left $ "Invalid priority value " ++ arg

itemDueByValueParser :: Parser LocalTime
itemDueByValueParser = option readDateTime $
    long "due-by"
    <> short 'b'
    <> metavar "DUEBY"
    <> help "due-by date/time"
    where
        readDateTime = eitherReader $ \arg ->
            case parseDateTimeMaybe arg of 
                (Just dateTime) -> Right dateTime
                Nothing -> Left $ "Date/time string must be in " ++ dateTimeFormat ++ " format"
        parseDateTimeMaybe = parseTimeM False defaultTimeLocale dateTimeFormat
        dateTimeFormat = "%Y/%m/%d %H:%M:%S"