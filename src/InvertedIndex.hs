module InvertedIndex where

  import Data.Char
  import Data.List
  import Data.Map (Map, fromList, (!))
  import Control.Applicative

  type Doc = String
  type Line = String
  type Word = String
  type Pos = Int
  type LineNumber = Int
  type Index = Map Word [(Pos, LineNumber)]

  both :: (a -> Bool) -> (a -> Bool) -> a -> Bool
  both = liftA2 (&&)

  numLines :: Doc -> [(Line, LineNumber)]
  numLines = flip zip [0..] . lines

  numWords :: Line -> [(Word, Pos)]
  numWords = (flip zip [0..]) . cleanWords . words

  cleanWords :: [Word] -> [Word]
  cleanWords = map (map toLower) .
               filter (not . null) .
               map (filter (both isLetter isAscii))

  allNumWords :: [(Line, LineNumber)] -> [(Word, Pos, LineNumber)]
  allNumWords = (>>= (\(l, i) -> map (\(w, p) -> (w, p, i)) $ numWords l))

  makeLists :: [(Word, Pos, LineNumber)] -> [(Word, [(Pos, LineNumber)])]
  makeLists = map (\((w, p, i)) -> (w, [(p, i)]))

  accumulate :: [(Word, [(Pos, LineNumber)])] -> [(Word, [(Pos, LineNumber)])]
  accumulate = foldl' f []
             where f [] x = [x]
                   f ((w, ls):xs) (w2, [l]) | w == w2 = (w, l:ls):xs
                   f ((w, ls):xs) (w2, [l]) | w /= w2 = (w2, [l]):((w, ls):xs)
                   f _ _ = fail "What the hell happened"

  createIndex :: Doc -> Index
  createIndex =  fromList . accumulate . makeLists . sort . allNumWords . numLines

  search :: Index -> String -> [(Pos, LineNumber)]
  search i = foldl1' (intersectBy (\x y -> snd x == snd y)) .
             map (i!) . cleanWords . words
