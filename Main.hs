import Data.Time (Day, TimeOfDay)
import Data.List ()
import Data.Maybe (fromJust)
import System.Command (runCommand, proc, waitForProcess, isSuccess)
import System.Environment (getArgs, getProgName)

readCalendarEvent :: String -> ((Day, TimeOfDay), String)
readCalendarEvent line = ((read $ head $ dateAndTime, read $ (head . tail) $ dateAndTime), eventText)
  where dateAndTime = words (takeWhile (/= '|') line)
        eventText   = dropWhile (== ' ') $ tail $ dropWhile (/= '|') line

showCalendarEvent :: ((Day, TimeOfDay), String) -> String
showCalendarEvent ((day, time), string) = "On " ++ show day ++ " at " ++ show time ++ ": " ++ string

listCalendarEntries :: IO ()
listCalendarEntries = do
  calendar <- readFile "cal.txt"
  printCalendar <- runCommand "cal"
  exitCode <- waitForProcess printCalendar
  
  if not $ isSuccess $ exitCode
    then putStrLn "Warning: cal seems not to work. Have you installed it correctly?"
    else return ()

  let events = map readCalendarEvent $ lines $ calendar
      printable = unlines $ map (((++) "- ") . showCalendarEvent) events

  putStr printable

addCalendarEntry :: IO ()
addCalendarEntry = return ()

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
       ("add":_)  -> addCalendarEntry
       _          -> printUsage
