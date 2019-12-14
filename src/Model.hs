module Model where

import Types
import qualified Data.Map as M (empty, lookup, insert)
import Data.Time hiding (Day, months)
import Data.List (sort)

-- @todo only change if the deletion occurs
removePunch :: PunchDate -> Timesheet -> Timesheet
removePunch punch timesheet = do
    let (yearNumber, monthNumber, dayNumber) = punchDate punch

    let year = yearLookup' yearNumber timesheet
    let month = monthLookup' monthNumber year
    let day = dayLookup' dayNumber month

    let oldPunches = punches day
    let punchTime = localTimeOfDay punch
    let newDay = if elem punchTime oldPunches then Day (filter (/= punchTime) oldPunches) else day

    let newMonth = Month (M.insert dayNumber newDay $ days month) $ allowances month
    let newYear = Year $ M.insert monthNumber newMonth (months year)
    Timesheet $ M.insert yearNumber newYear (years timesheet)

addPunch :: PunchDate -> Timesheet -> Timesheet
addPunch punch timesheet = do
    let (yearNumber, monthNumber, dayNumber) = punchDate punch

    let year = yearLookup' yearNumber timesheet
    let month = monthLookup' monthNumber year
    let day = dayLookup' dayNumber month

    let oldPunches = punches day
    let punchTime = localTimeOfDay punch
    let newDay = if elem punchTime oldPunches then day else Day $ punchTime : oldPunches

    let newMonth = Month (M.insert dayNumber newDay $ days month) $ allowances month
    let newYear = Year $ M.insert monthNumber newMonth (months year)
    Timesheet $ M.insert yearNumber newYear (years timesheet)

dayLookup :: DayNumber -> Month -> Maybe Day
dayLookup dayNumber month = M.lookup dayNumber $ days month

dayLookup' :: DayNumber -> Month -> Day
dayLookup' dayNumber month = case dayLookup dayNumber month of
    Nothing -> Day []
    Just x -> x

monthLookup :: MonthNumber -> Year -> Maybe Month
monthLookup monthNumber year = M.lookup monthNumber $ months year

monthLookup' :: MonthNumber -> Year -> Month
monthLookup' monthNumber year = case monthLookup monthNumber year of
    Nothing -> Month M.empty []
    Just x -> x

monthLookup'' :: YearNumber -> MonthNumber -> Timesheet -> Maybe Month
monthLookup'' yearNumber monthNumber timesheet = yearLookup yearNumber timesheet >>= monthLookup monthNumber

yearLookup :: YearNumber -> Timesheet -> Maybe Year
yearLookup yearNumber timesheet = M.lookup yearNumber $ years timesheet

yearLookup' :: YearNumber -> Timesheet -> Year
yearLookup' yearNumber timesheet = case yearLookup yearNumber timesheet of
    Nothing -> Year M.empty
    Just x -> x

punchDate :: PunchDate -> (YearNumber, MonthNumber, Int)
punchDate punch = let (a,b,c) = (toGregorian . localDay) punch in (fromIntegral a, b, c)

calculateBalance :: Day -> (Duration, Duration, Duration)
calculateBalance day = (worked, left, balance)
    where
        now = TimeOfDay 23 23 0
        zippedPunches = zip2WithDefault sortedPunches now
        sortedPunches = reverse $ sort $ punches day
        worked = foldr sumDuration zero (map (uncurry diffTime) zippedPunches)
        left = if worked >= workDay then zero else diffTime workDay worked
        balance = if worked >= workDay then worked else TimeOfDay (negate $ todHour left) (todMin left) (todSec left)
        workDay = TimeOfDay 8 0 0
        zero = TimeOfDay 0 0 0

sumDuration :: Duration -> Duration -> Duration
sumDuration a b = timeToTimeOfDay $ (timeOfDayToTime a) + (timeOfDayToTime b)

diffTime :: TimeOfDay -> TimeOfDay -> Duration
diffTime a b = timeToTimeOfDay $ (timeOfDayToTime a) - (timeOfDayToTime b)

zip2WithDefault :: Ord a => [a] -> a -> [(a,a)]
zip2WithDefault [] _ = []
zip2WithDefault [x] d = if x > d then [(d,x)] else [(x,d)]
zip2WithDefault (a:b:xs) d = (a,b) : zip2WithDefault xs d