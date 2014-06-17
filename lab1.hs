import Data.List
import Data.Char

inpt = "Dogs DOGS Child ChIlD Hello"
maxlength = 80

hist = toBar . wordsWithCount
  where wordsWithCount x = reverse . sort . map (\x -> (fromIntegral (length x), head x)) . group . words $ map toLower x
        toBar xs = map (\x -> (replicate (barLength x xs) '#', snd x)) xs
        barLength x xs = round $ fst x * maxlength / highestCount xs
        highestCount xs = fst $ maximum xs
