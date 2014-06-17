import Control.Monad
import Data.List
import Data.Char
import System.IO

main = do
  input <- getLine
  putStrLn (histogram input)

histogram :: String -> String
histogram = intercalate "\n" . toBar . wordsWithCount
  where wordsWithCount x = reverse . sort . map (\x -> (genericLength x, head x)) . group . sort . words $ map toLower x
        toBar xs = map (\x -> wordWithPadding x xs ++ replicate (barLength x xs) '#') xs
        barLength x xs = round $ fst x * maxLength xs / highestCount xs
        wordWithPadding x xs = snd x ++ genericReplicate (longestWord xs - genericLength (snd x) + 1) ' '
        highestCount xs = fst $ maximum xs
        longestWord xs = maximum $ map (\x -> genericLength (snd x)) xs
        maxLength xs = 80 - longestWord xs - 1 -- always allow a padding of 1

-- remaining requirements:
-- optional input as command line argument
-- a word with a bar length of 0 should not be printed
-- strip interpunctuation
