-- ockcal - the simplest possible calendar that can work
-- Copyright (C) 2015 Lukas Epple aka sternenseemann
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

import Data.Time (getCurrentTime, LocalTime(..), utcToLocalTime, getCurrentTimeZone)
import Data.List (sort, insert, findIndex)
import Data.Maybe (fromJust)
import System.Command (runCommand, waitForProcess, isSuccess)
import System.Environment (getArgs, getProgName)
import System.Directory (removeFile, doesFileExist)
import Control.Monad (when, unless)

-- config

calendarFile :: FilePath
calendarFile = "cal.txt"

temporaryFile :: FilePath
temporaryFile = ".cal.tmp"

-- the file format of the calendarFile is as follows:
-- YYYY-MM-DD HH:MM:SS | <event title>
--
-- it currently gets parsed using read
-- I will evaluate if there's any problem doing this

-- internal stuff

data CalendarEvent = CalendarEvent LocalTime String
  deriving (Show, Read, Eq, Ord)

-- construct a CalendarEvent from a line in the calendarFile
parseCalendarEvent :: String -> CalendarEvent
parseCalendarEvent line = CalendarEvent (LocalTime (read $ head dateAndTime) (read $ (head . tail) dateAndTime)) eventText
  where dateAndTime = words $ takeWhile (/= '|') line
        eventText   = dropWhile (== ' ') $ tail $ dropWhile (/= '|') line

-- opposite of parseCalendarEvent
showCalendarEvent :: CalendarEvent -> String
showCalendarEvent (CalendarEvent (LocalTime day time) text) = show day ++ " " ++ show time ++ " | " ++ text

-- creates a string showing a CalendarEvent in a human readable form
prettyCalendarEvent :: CalendarEvent -> String
prettyCalendarEvent (CalendarEvent (LocalTime day time) string) = "On " ++ show day ++ " at " ++ show time ++ ": " ++ string

prettyCalendarEventListing :: [CalendarEvent] -> Int -> String
prettyCalendarEventListing events enumstart = unlines $ map (\x -> fst x ++ ". " ++ snd x) $
                                               zip (map show [enumstart .. enumstart + length events]) $ map prettyCalendarEvent events

-- helper IO functions

readCalendarEventsFromFile :: FilePath -> IO [CalendarEvent]
readCalendarEventsFromFile file = do
  calendar <- readFile file
  return $ sort $ map parseCalendarEvent $ lines calendar

copyFile :: FilePath -> FilePath -> IO ()
copyFile from to = writeFile to =<< readFile from

-- IO actions for the subcommands

listCalendarEntries :: (LocalTime -> LocalTime -> Bool) -> IO ()
listCalendarEntries timeRelation = do
  localTimeZone <- getCurrentTimeZone
  now           <- getCurrentTime
  calendar      <- readCalendarEventsFromFile calendarFile

  -- use the coreutil cal to print a nice calendar
  prettyCalendar <- runCommand "cal"
  exitCode       <- waitForProcess prettyCalendar

  unless (isSuccess exitCode) (error "Unable to call the coreutil 'cal'")

  -- generate a simple listing of the upcoming events noted in the calendar
  -- Note: we asume the input list is sorted. this is done by readCalendarEventsFromFile
  let filterFun (CalendarEvent time _) = time `timeRelation` utcToLocalTime localTimeZone now
      matchingEvents = filter filterFun calendar
      enumStart      = if null matchingEvents then 0 else (+) 1 $ fromJust $ findIndex filterFun calendar
  putStr $ prettyCalendarEventListing matchingEvents enumStart

addCalendarEntry :: [String] -> IO ()
addCalendarEntry [day, time, text] = do
  -- copy the file to a temporary location
  -- see README#FAQ
  copyFile calendarFile temporaryFile

  calendar <- readCalendarEventsFromFile temporaryFile
  writeFile calendarFile $ unlines $ map showCalendarEvent $ insert (CalendarEvent (LocalTime (read day) (read time)) text) calendar

addCalendarEntry _ = error "Incorrect number of arguments"

deleteCalendarEntry :: [String] -> IO ()
deleteCalendarEntry [num] = do
  copyFile calendarFile temporaryFile

  calendar <- readCalendarEventsFromFile temporaryFile
  writeFile calendarFile $ unlines $ map showCalendarEvent $ take (read num - 1) calendar ++ drop (read num) calendar

deleteCalendarEntry _ = error "Incorrect number of arguments"

printUsage :: IO ()
printUsage = do
  prog <- getProgName
  putStrLn $ prog ++ " [command]\n\n\tlist - list all upcoming calendar items\n\tpast - list all past calendar events\n\tadd YYYY-MM-DD HH:MM:SS title - add a event to the calendar"

main :: IO ()
main = do
  args <- getArgs

  case args of
       []           -> listCalendarEntries (>=)
       ("list":_)   -> listCalendarEntries (>=)
       ("past":_)   -> listCalendarEntries (<=)
       ("add":info) -> addCalendarEntry info
       ("del":info) -> deleteCalendarEntry info
       _            -> printUsage

  -- delete the temporary file if it exists
  temporaryFileExists <- doesFileExist temporaryFile
  when temporaryFileExists $ removeFile temporaryFile
