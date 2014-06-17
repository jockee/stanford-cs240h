import Control.Monad
import Data.List
import Data.Char
import System.IO

inpt = "Dogs DOGS Child ChIlD Hello"

main = do
  input <- getLine
  putStrLn (histogram input)

histogram :: String -> String
histogram = intercalate "\n" . toBar . wordsWithCount
  where wordsWithCount x = reverse . sort . map (\x -> (genericLength x, head x)) . group . words $ map toLower x
        toBar xs = map (\x -> wordWithPadding x xs ++ replicate (barLength x xs) '#') xs
        barLength x xs = round $ fst x * maxLength / highestCount xs
        wordWithPadding x xs = snd x ++ genericReplicate (longestWord xs - genericLength (snd x)) ' '
        highestCount xs = fst $ maximum xs
        longestWord xs = 10
        maxLength = 80
