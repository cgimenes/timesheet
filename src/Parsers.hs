module Parsers where

import Options.Applicative hiding (infoParser)
import Types
import Data.Time (LocalTime, defaultTimeLocale, parseTimeM)

infoParser :: Parser Command
infoParser = pure Info

initParser :: Parser Command
initParser = pure Init

getMonthParser :: Parser Command
getMonthParser = GetMonth <$> yearNumberParser <*> monthNumberParser

punchParser :: Parser Command
punchParser = Punch <$> punchDateParser

deletePunchParser :: Parser Command
deletePunchParser = DeletePunch <$> punchDateParser

-- addItemParser :: Parser Item
-- addItemParser = Item
--     <$> argument str (metavar "TITLE" <> help "title")
--     <*> optional itemDescriptionValueParser
--     <*> optional itemPriorityValueParser
--     <*> optional itemDueByValueParser

-- viewParser :: Parser Command
-- viewParser = View <$> itemIndexParser

-- updateParser :: Parser Command
-- updateParser = Update <$> itemIndexParser <*> updateItemParser

-- updateItemParser :: Parser ItemUpdate
-- updateItemParser = ItemUpdate
--     <$> optional updateItemTitleParser
--     <*> optional updateItemDescriptionParser
--     <*> optional updateItemPriorityParser
--     <*> optional updateItemDueByParser

-- updateItemTitleParser :: Parser ItemTitle
-- updateItemTitleParser = itemTitleValueParser

-- updateItemDescriptionParser :: Parser ItemDescription
-- updateItemDescriptionParser = 
--     Just <$> itemDescriptionValueParser
--     <|> flag' Nothing (long "clear-desc")

-- updateItemPriorityParser :: Parser ItemPriority
-- updateItemPriorityParser = 
--     Just <$> itemPriorityValueParser
--     <|> flag' Nothing (long "clear-priority")    

-- updateItemDueByParser :: Parser ItemDueBy
-- updateItemDueByParser = 
--     Just <$> itemDueByValueParser
--     <|> flag' Nothing (long "clear-due-by")    

commandParser:: Parser Command
commandParser = subparser $ mconcat
    [ command "info" (info infoParser (progDesc "Show info"))
    , command "init" (info initParser (progDesc "Initialize database"))
    , command "get-month" (info getMonthParser (progDesc "Get month"))
    , command "punch" (info punchParser (progDesc "Add punch"))
    , command "delete-punch" (info deletePunchParser (progDesc "Delete punch"))
    -- , command "createAllowance" (info createAllowanceParser (progDesc "Create allowance"))
    -- , command "updateAllowance" (info updateAllowanceParser (progDesc "Update allowance"))
    -- , command "deleteAllowance" (info deleteAllowanceParser (progDesc "Delete allowance"))
    ]

optionsParser :: Parser Options
optionsParser = Options
    <$> dataPathParser
    <*> commandParser

dataPathParser :: Parser FilePath
dataPathParser = strOption $
    value defaultDataPath
    <> long "data-path"
    <> short 'd'
    <> metavar "DATAPATH"
    <> help ("path to data file (default " ++ defaultDataPath ++ ")")
    where 
        defaultDataPath = "~/.timesheet.yaml"

-- itemIndexParser :: Parser ItemIndex
-- itemIndexParser = argument auto $
--     metavar "ITEMINDEX" 
--     <> help "index of item"

-- itemTitleValueParser :: Parser String
-- itemTitleValueParser = strOption $
--     long "title"
--     <> short 't'
--     <> metavar "TITLE"
--     <> help "title"

-- itemDescriptionValueParser :: Parser String
-- itemDescriptionValueParser = strOption $
--     long "desc"
--     <> short 'd'
--     <> metavar "DESCRIPTION"
--     <> help "description"

-- itemPriorityValueParser :: Parser Priority
-- itemPriorityValueParser = option (eitherReader readPriority) $
--     long "priority"
--     <> short 'p'
--     <> metavar "PRIORITY"
--     <> help "priority (1: Low, 2: Normal, 3: High)"
--     where 
--         readPriority arg = case arg of
--             "1" -> Right Low
--             "2" -> Right Normal
--             "3" -> Right High
--             _ -> Left $ "Invalid priority value " ++ arg

-- itemDueByValueParser :: Parser LocalTime
-- itemDueByValueParser = option readDateTime $
--     long "due-by"
--     <> short 'b'
--     <> metavar "DUEBY"
--     <> help "due-by date/time"
--     where
--         readDateTime = eitherReader $ \arg ->
--             case parseDateTimeMaybe arg of 
--                 (Just dateTime) -> Right dateTime
--                 Nothing -> Left $ "Date/time string must be in " ++ dateTimeFormat ++ " format"
--         parseDateTimeMaybe = parseTimeM False defaultTimeLocale dateTimeFormat
--         dateTimeFormat = "%Y/%m/%d %H:%M:%S"

yearNumberParser :: Parser YearNumber
yearNumberParser = option auto $
    long "year"
    <> short 'y'
    <> metavar "YEAR"
    <> help "year"

monthNumberParser :: Parser MonthNumber
monthNumberParser = option auto $
    long "month"
    <> short 'm'
    <> metavar "MONTH"
    <> help "month"

punchDateParser :: Parser LocalTime
punchDateParser = option readDateTime $
    long "punch-date"
    <> short 'p'
    <> metavar "PUNCHDATE"
    <> help "punch-date date/time"
    where
        readDateTime = eitherReader $ \arg ->
            case parseDateTimeMaybe arg of 
                (Just dateTime) -> Right dateTime
                Nothing -> Left $ "Date/time string must be in " ++ dateTimeFormat ++ " format"
        parseDateTimeMaybe = parseTimeM False defaultTimeLocale dateTimeFormat
        dateTimeFormat = "%Y-%m-%d %H:%M"