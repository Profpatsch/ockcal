import Data.Time (Day, TimeOfDay)
import Data.List ()
import System.Command (runCommand, waitForProcess, isSuccess)
import System.Environment (getArgs, getProgName)
import Control.Monad (unless)

calendarFile :: String
calendarFile = "cal.txt"

-- the file format of the calendarFile is as follows:
-- YYYY-MM-DD HH:MM:SS | <event title>
--
-- it currently gets parsed using read
-- I will evaluate if there's any problem doing this

readCalendarEvent :: String -> ((Day, TimeOfDay), String)
readCalendarEvent line = ((read $ head dateAndTime, read $ (head . tail) dateAndTime), eventText)
  where dateAndTime = words (takeWhile (/= '|') line)
        eventText   = dropWhile (== ' ') $ tail $ dropWhile (/= '|') line

showCalendarEvent :: ((Day, TimeOfDay), String) -> String
showCalendarEvent ((day, time), string) = "On " ++ show day ++ " at " ++ show time ++ ": " ++ string

listCalendarEntries :: IO ()
listCalendarEntries = do
  calendar <- readFile calendarFile

  -- use the coreutil cal to print a nice calendar
  printCalendar <- runCommand "cal"
  exitCode <- waitForProcess printCalendar
  
  unless (not $ isSuccess exitCode) (error "Unable to call the coreutil 'cal'")

  -- generate a simple listning of the events noted in the calendar
  let events = map readCalendarEvent $ lines calendar
      printable = unlines $ map (("- " ++) . showCalendarEvent) events

  putStr printable

addCalendarEntry :: [String] -> IO ()
addCalendarEntry _ = return ()

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
