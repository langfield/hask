module Meetup (Weekday(..), Schedule(..), meetupDay) where

import Data.Fixed (mod')
import Data.Time.Calendar
  (Day, DayOfWeek, addDays, addGregorianMonthsClip, dayOfWeek, fromGregorian)
import qualified Data.Time.Calendar as Cal

data Weekday = Monday
             | Tuesday
             | Wednesday
             | Thursday
             | Friday
             | Saturday
             | Sunday

data Schedule = First
              | Second
              | Third
              | Fourth
              | Last
              | Teenth

weekdayToDayOfWeek :: Weekday -> DayOfWeek
weekdayToDayOfWeek Monday    = Cal.Monday
weekdayToDayOfWeek Tuesday   = Cal.Tuesday
weekdayToDayOfWeek Wednesday = Cal.Wednesday
weekdayToDayOfWeek Thursday  = Cal.Thursday
weekdayToDayOfWeek Friday    = Cal.Friday
weekdayToDayOfWeek Saturday  = Cal.Saturday
weekdayToDayOfWeek Sunday    = Cal.Sunday

-- | @dayOfWeekDiff a b = a - b@ in range 0 to 6.
-- The number of days from b to the next a.
dayOfWeekDiff :: DayOfWeek -> DayOfWeek -> Int
dayOfWeekDiff a b = mod' (fromEnum a - fromEnum b) 7

-- | The first day-of-week on or after some day
firstDayOfWeekOnAfter :: DayOfWeek -> Day -> Day
firstDayOfWeekOnAfter dw d =
  addDays (toInteger $ dayOfWeekDiff dw $ dayOfWeek d) d

meetupDay :: Schedule -> Weekday -> Integer -> Int -> Day
meetupDay schedule weekday year month =
  firstDayOfWeekOnAfter dw $ case schedule of
    First  -> first
    Second -> addDays 7 first
    Third  -> addDays 14 first
    Fourth -> addDays 21 first
    Last   -> addDays (-7) nextFirst
    Teenth -> addDays 12 first
  where
    dw    = weekdayToDayOfWeek weekday
    first = fromGregorian year month 1
    nextFirst = addGregorianMonthsClip 1 first
