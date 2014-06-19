import Data.Char
import Control.Monad
import Data.List
import System.IO
import System.Environment
import Control.Arrow

minimumPadding :: Integer
minimumPadding = 1

screenWidth :: Integer
screenWidth = 80

main :: IO ()
main = do
  files <- getArgs
  input <- if not $ null files
            then unwords `liftM` mapM readFile files
            else getLine
  putStrLn $ histogram input

histogram :: String -> String
histogram = toBars . filterOutLittleOccuring . wordsWithCount . wordsWithoutInterpunctuation
  where wordsWithoutInterpunctuation = filter $ not . isPunctuation

toBars :: RealFrac a => [(a, String)] -> String
toBars xs = intercalate "\n" $ fmap (\x -> wordWithPadding x xs ++ genericReplicate (barLength x xs) '#') xs

filterOutLittleOccuring :: (Ord b, RealFrac a) => [(a, [b])] -> [(a, [b])]
filterOutLittleOccuring xs = filter (\x -> barLength x xs > 0) xs

barLength :: (Integral b, Ord b1, RealFrac a) => (a, t) -> [(a, [b1])] -> b
barLength (count, _) xs = round $ count * maxLength xs / highestCount xs
  where highestCount = fst . maximum

wordWithPadding :: (t, String) -> [(a, [b])] -> String
wordWithPadding (_, word) xs = word ++ genericReplicate (longestWord xs - genericLength word + minimumPadding) ' '

longestWord :: [(a, [b])] -> Integer
longestWord = maximum . fmap (genericLength . snd)

maxLength :: Num b => [(a, [b1])] -> b
maxLength xs = fromIntegral $ screenWidth - minimumPadding - longestWord xs

wordsWithCount :: String -> [(Double, String)]
wordsWithCount = sortBy (flip compare) . fmap (genericLength &&& head) . groupedWords
  where groupedWords = group . sort . words . fmap toLower
