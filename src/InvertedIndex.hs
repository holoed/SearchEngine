module InvertedIndex where

  import Data.Function
  import Data.Char
  import Data.List
  import Data.Map (Map, fromList, (!))
  import Control.Applicative

  type Doc = String
  type Line = String
  type Word = String
  type LineNumber = Int
  type Index = Map Word [LineNumber]
  type Frequency = Int

  both :: (a -> Bool) -> (a -> Bool) -> a -> Bool
  both = liftA2 (&&)

  numLines :: Doc -> [(Line, LineNumber)]
  numLines = flip zip [0..] . lines

  cleanWords :: [Word] -> [Word]
  cleanWords = filter (not . null) . map (filter (both isLetter isAscii))

  allNumWords :: [(Line, LineNumber)] -> [(Word, LineNumber)]
  allNumWords = (>>= (\(l, i) -> map (\w -> (w, i)) $ cleanWords $ words l))

  makeLists :: [(Word, LineNumber)] -> [(Word, [LineNumber])]
  makeLists = map (\((w, i)) -> (w, [i]))

  accumulate :: [(Word, [LineNumber])] -> [(Word, [LineNumber])]
  accumulate = foldl' f []
             where f [] x = [x]
                   f ((w, ls):xs) (w2, [l]) | w == w2 = (w, l:ls):xs
                   f ((w, ls):xs) (w2, [l]) | w /= w2 = (w2, [l]):((w, ls):xs)
                   f _ _ = fail "What the hell happened"

  createIndex :: Doc -> Index
  createIndex =  fromList . accumulate . makeLists . sort . allNumWords . numLines

  frequencies :: (Eq a, Ord a) => [a] -> [(a, Frequency)]
  frequencies = sortBy (flip compare `on` snd) . foldl' count [] . sort
    where
      count [] x = [(x, 1)]
      count ((y, i):xs) x | x == y = (y, i + 1):xs
      count ((y, i):xs) x          = (x, 1):((y, i):xs)

  search :: Index -> String -> [(LineNumber, Frequency)]
  search i = frequencies . concatMap (\w -> i!w) . cleanWords . words
