import Data.Time (Day, TimeOfDay)
import Data.List (sort, insert)
import System.Command (runCommand, waitForProcess, isSuccess)
import System.Environment (getArgs, getProgName)
import System.Directory (removeFile, doesFileExist)
import Control.Monad (unless, when)

-- config

calendarFile :: FilePath
calendarFile = "cal.txt"

temporaryFile :: FilePath
temporaryFile = ".cal_tmp.txt"

-- the file format of the calendarFile is as follows:
-- YYYY-MM-DD HH:MM:SS | <event title>
--
-- it currently gets parsed using read
-- I will evaluate if there's any problem doing this

-- internal stuff

data CalendarEvent = CalendarEvent Day TimeOfDay String
  deriving (Show, Read, Eq, Ord)

-- construct a CalendarEvent from a line in the calendarFile
parseCalendarEvent :: String -> CalendarEvent
parseCalendarEvent line = CalendarEvent (read $ head dateAndTime) (read $ (head . tail) dateAndTime) eventText
  where dateAndTime = words (takeWhile (/= '|') line)
        eventText   = dropWhile (== ' ') $ tail $ dropWhile (/= '|') line

-- opposite of parseCalendarEvent
showCalendarEvent :: CalendarEvent -> String
showCalendarEvent (CalendarEvent day time text) = show day ++ " " ++ show time ++ " | " ++ text

-- creates a string showing a CalendarEvent in a human readable form
prettyCalendarEvent :: CalendarEvent -> String
prettyCalendarEvent (CalendarEvent day time string) = "On " ++ show day ++ " at " ++ show time ++ ": " ++ string

-- helper IO functions

readCalendarEventsFromFile :: FilePath -> IO [CalendarEvent]
readCalendarEventsFromFile file = do
  calendar <- readFile file
  return $ sort $ map parseCalendarEvent $ lines calendar

copyFile :: FilePath -> FilePath -> IO ()
copyFile from to = writeFile to =<< readFile from

-- IO actions for the subcommands

listCalendarEntries :: IO ()
listCalendarEntries = do
  calendar <- readCalendarEventsFromFile calendarFile

  -- use the coreutil cal to print a nice calendar
  prettyCalendar <- runCommand "cal"
  exitCode <- waitForProcess prettyCalendar

  unless (isSuccess exitCode) (error "Unable to call the coreutil 'cal'")

  -- generate a simple listing of the events noted in the calendar
  let prettyEvents = unlines $ map (("- " ++) . prettyCalendarEvent) calendar

  putStr prettyEvents

addCalendarEntry :: [String] -> IO ()
addCalendarEntry [day, time, text] = do
  -- copy the file to a temporary location
  -- see README#FAQ
  copyFile calendarFile temporaryFile

  calendar <- readCalendarEventsFromFile temporaryFile
  writeFile calendarFile $ unlines $ map showCalendarEvent $ insert (CalendarEvent (read day) (read time) text) calendar

addCalendarEntry _ = error "Incorrect number of arguments"

printUsage :: IO ()
printUsage = do
  prog <- getProgName
  putStrLn $ prog ++ " [list|add]\n\n\tlist - list all calendar items\n\tadd YYYY-MM-DD HH:MM:SS title - add a event to the calendar"

main :: IO ()
main = do
  args <- getArgs

  case args of
       []         -> listCalendarEntries
       ("list":_) -> listCalendarEntries
       ("add":info)  -> addCalendarEntry info
       _          -> printUsage

  -- delete the temporary file if it exists
  temporaryFileExists <- doesFileExist temporaryFile
  when temporaryFileExists $ removeFile temporaryFile
