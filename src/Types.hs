{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Types where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Data.Time (LocalTime, NominalDiffTime)

type PunchDate = LocalTime
type Duration = NominalDiffTime
type AllowanceReason = String
type YearNumber = Int
type MonthNumber = Int
type AllowanceIndex = Int

data Database = Database [Year] deriving (Generic, Show)
instance ToJSON Database
instance FromJSON Database

data Year = Year YearNumber [Month] deriving (Generic, Show)
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

data Date = Date
    { punchs :: [PunchDate]
    , worked :: Duration
    , left :: Duration
    , balance :: Duration
    } deriving (Generic, Show)
instance ToJSON Date
instance FromJSON Date

data Month = Month
    { monthNumber :: MonthNumber
    , dates :: [Date]
    , allowances :: [Allowance]
    , monthBalance :: Duration
    , totalBalance :: Duration
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
    | GetMonth Year MonthNumber 
    | Punch PunchDate
    | DeletePunch PunchDate
    | CreateAllowance Allowance
    | UpdateAllowance AllowanceIndex AllowanceUpdate
    | DeleteAllowance AllowanceIndex 
    deriving Show