import Data.Time (Day, TimeOfDay, getCurrentTime)
import Data.List ()
import Data.Maybe (fromJust)
import System.Command (runCommand, proc, waitForProcess, isSuccess)

readCalendarEvent :: String -> ((Day, TimeOfDay), String)
readCalendarEvent line = ((read $ head $ dateAndTime, read $ (head . tail) $ dateAndTime), eventText)
  where dateAndTime = words (takeWhile (/= '|') line)
        eventText   = dropWhile (== ' ') $ tail $ dropWhile (/= '|') line

showCalendarEvent :: ((Day, TimeOfDay), String) -> String
showCalendarEvent ((day, time), string) = "On " ++ show day ++ " at " ++ show time ++ ": " ++ string

main :: IO ()
main = do
--  now <- getCurrentTime
  printCalendar <- runCommand "cal"
  exitCode <- waitForProcess printCalendar
  
  if not $ isSuccess $ exitCode
    then putStrLn "Warning: cal seems not to work. Have you installed it correctly?"
    else return ()

  calendar <- readFile "cal.txt"

  let events = map readCalendarEvent $ lines $ calendar
      printable = unlines $ map (((++) "- ") . showCalendarEvent) events

  putStr printable
