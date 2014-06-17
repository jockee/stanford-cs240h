import Data.List
import Data.Char

inpt = "Dogs DOGS Child ChIlD Hello"
maxlength = 80

hist = wordsWithCount
  where wordsWithCount x = reverse . sort . map (\x -> (fromIntegral (length x), head x)) . group . words $ map toLower x
