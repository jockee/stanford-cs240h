import Data.Char
import Control.Monad
import Data.List
import System.IO
import System.Environment
import Control.Arrow

minimumPadding = 1
screenWidth = 80

main = do
  files <- getArgs
  input <- if not $ null files
            then unwords `liftM` mapM readFile files
            else getLine
  putStrLn $ histogram input

histogram = intercalate "\n" . toBar . filterOutLittleOccuring . wordsWithCount . wordsWithoutInterpunctuation
  where wordsWithoutInterpunctuation = filter $ not . isPunctuation

toBar xs = fmap (\x -> wordWithPadding x xs ++ genericReplicate (barLength x xs) '#') xs
filterOutLittleOccuring xs = filter (\x -> barLength x xs > 0) xs 
barLength (count, _) xs = round $ count * maxLength xs / highestCount xs
  where highestCount = fst . maximum

wordWithPadding (_, word) xs = word ++ genericReplicate (longestWord xs - genericLength word + minimumPadding) ' '
longestWord = maximum . fmap (genericLength . snd)
maxLength xs = fromIntegral $ screenWidth - minimumPadding - longestWord xs

wordsWithCount = reverse . sort . fmap (genericLength &&& head) . groupedWords
  where groupedWords = group . sort . words . fmap toLower
