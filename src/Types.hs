{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Types where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Data.Time (LocalTime, NominalDiffTime, TimeOfDay)
import Data.Map (Map)
import Data.List (intercalate)

type Duration = TimeOfDay
type PunchHour = Int
type PunchMinute = Int
type AllowanceReason = String
type YearNumber = Int
type MonthNumber = Int
type DayNumber = Int
type AllowanceIndex = Int
type YearMonths = Map MonthNumber Month
type Years = Map YearNumber Year
type PunchDate = LocalTime
type MonthDays = Map DayNumber Day
type PunchTime = TimeOfDay

data Timesheet = Timesheet 
    { years :: Years
    } deriving (Generic, Show)
instance ToJSON Timesheet
instance FromJSON Timesheet

data Year = Year 
    { months :: YearMonths
    } deriving (Generic, Show)
instance ToJSON Year
instance FromJSON Year

data Period = Period LocalTime LocalTime deriving (Generic, Show)
instance ToJSON Period
instance FromJSON Period

data Allowance = DurationAllowance 
    { duration :: Duration
    , reason :: AllowanceReason
    } | PeriodAllowance 
    { period :: Period
    , reason :: AllowanceReason
    } deriving (Generic, Show)
instance ToJSON Allowance
instance FromJSON Allowance

    -- , allowance :: Maybe Allowance
data Day = Day
    { punches :: [PunchTime]
    } deriving (Generic)
instance ToJSON Day
instance FromJSON Day
instance Show Day where
    show d = "Punches: " ++ intercalate "  " (map show $ punches d)

data Month = Month
    { days :: MonthDays
    , allowances :: [Allowance]
    } deriving (Generic, Show)
instance ToJSON Month
instance FromJSON Month

data AllowanceUpdate = DurationAllowanceUpdate
    { durationUpdate :: Maybe Duration
    , reasonUpdate :: Maybe AllowanceReason
    } | PeriodAllowanceUpdate
    { periodUpdate :: Maybe Period
    , reasonUpdate :: Maybe AllowanceReason
    } deriving Show

data Options = Options FilePath Command deriving Show

data Command = 
    Info
    | Init
    | GetMonth YearNumber MonthNumber 
    | Punch PunchDate
    | DeletePunch PunchDate
    | CreateAllowance Allowance
    | UpdateAllowance AllowanceIndex AllowanceUpdate
    | DeleteAllowance AllowanceIndex 
    deriving Show