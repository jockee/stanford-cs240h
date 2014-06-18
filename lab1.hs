import Data.Char
import Control.Monad
import Data.List
import System.IO
import System.Environment

minimumPadding = 1
screenWidth = 80


main = do
  args <- getArgs
  if hasArgs args
    then fromArgs args
    else fromCommandLine
  where hasArgs args = not $ null args

fromCommandLine = do
  input <- getLine
  putStrLn (type input)
  printHistogram input

fromArgs args = do
  input <- head $ map readFile args
  --input <- readFile $ unwords args
  printHistogram input

printHistogram = putStrLn . histogram

histogram :: String -> String
histogram = intercalate "\n" . toBar . filterOutLittleOccuring . wordsWithCount . wordsWithoutInterpunctuation
  where wordsWithoutInterpunctuation = filter $ not . isPunctuation
        wordsWithCount x = reverse . sort . map (\x -> (genericLength x, head x)) $ sortedWords x
        sortedWords x = group . sort . words $ map toLower x
        toBar xs = map (\x -> wordWithPadding x xs ++ genericReplicate (barLength x xs) '#') xs
        barLength x xs = round $ fst x * maxLength xs / highestCount xs
        wordWithPadding x xs = snd x ++ genericReplicate (longestWord xs - genericLength (snd x) + minimumPadding) ' '
        longestWord xs = maximum $ map (genericLength . snd) xs
        filterOutLittleOccuring xs = filter (\x -> barLength x xs > 0) xs
        highestCount xs = fst $ maximum xs
        maxLength xs = fromIntegral $ screenWidth - minimumPadding - longestWord xs

-- remaining requirements:
-- read multiple files
