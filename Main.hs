import Data.Time (Day, TimeOfDay)
import Data.List (sort)
import System.Command (runCommand, waitForProcess, isSuccess)
import System.Environment (getArgs, getProgName)
import Control.Monad (unless)

-- config

calendarFile :: FilePath
calendarFile = "cal.txt"

-- the file format of the calendarFile is as follows:
-- YYYY-MM-DD HH:MM:SS | <event title>
--
-- it currently gets parsed using read
-- I will evaluate if there's any problem doing this

-- internal stuff

data CalendarEvent = CalendarEvent Day TimeOfDay String
  deriving (Show, Read, Eq, Ord)

parseCalendarEvent :: String -> CalendarEvent
parseCalendarEvent line = CalendarEvent (read $ head dateAndTime) (read $ (head . tail) dateAndTime) eventText
  where dateAndTime = words (takeWhile (/= '|') line)
        eventText   = dropWhile (== ' ') $ tail $ dropWhile (/= '|') line

-- opposite of parseCalendarEvent
showCalendarEvent :: CalendarEvent -> String
showCalendarEvent (CalendarEvent day time text) = show day ++ " " ++ show time ++ " | " ++ text

printCalendarEvent :: CalendarEvent -> String
printCalendarEvent (CalendarEvent day time string) = "On " ++ show day ++ " at " ++ show time ++ ": " ++ string

readCalendarEventsFromFile :: FilePath -> IO [CalendarEvent]
readCalendarEventsFromFile file = do
  calendar <- readFile file
  return $ sort $ map parseCalendarEvent $ lines calendar

-- IO actions for the subcommands

listCalendarEntries :: IO ()
listCalendarEntries = do
  calendar <- readCalendarEventsFromFile calendarFile

  -- use the coreutil cal to print a nice calendar
  printCalendar <- runCommand "cal"
  exitCode <- waitForProcess printCalendar

  unless (isSuccess exitCode) (error "Unable to call the coreutil 'cal'")

  -- generate a simple listing of the events noted in the calendar
  let prettyEvents = unlines $ map (("- " ++) . printCalendarEvent) calendar

  putStr prettyEvents

addCalendarEntry :: [String] -> IO ()
addCalendarEntry [day, time, text] = appendFile calendarFile $ showCalendarEvent $ CalendarEvent (read day) (read time) text
addCalendarEntry _ = error "Incorrect number of arguments"

printUsage :: IO ()
printUsage = do
  prog <- getProgName
  putStrLn $ prog ++ " [list|add]"

main :: IO ()
main = do
  args <- getArgs

  case args of
       []         -> listCalendarEntries
       ("list":_) -> listCalendarEntries
       ("add":info)  -> addCalendarEntry info
       _          -> printUsage
