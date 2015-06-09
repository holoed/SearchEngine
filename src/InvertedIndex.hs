module InvertedIndex where

  import Prelude hiding (lookup)
  import Data.Function
  import Data.Char
  import Data.List hiding (lookup)
  import Data.Map (Map, fromList, lookup, toList, filterWithKey)
  import Control.Applicative

  type Doc = String
  type Line = String
  type Word = String
  type Pos = Int
  type LineNumber = Int 
  type Index = Map Word [(Pos, LineNumber)]

  data IndexedWord = IndexedWord { word:: Word, pos:: Pos, line:: LineNumber }

  both :: (a -> Bool) -> (a -> Bool) -> a -> Bool
  both = liftA2 (&&)

  numLines :: Doc -> [(Line, LineNumber)]
  numLines = flip zip [0..] . lines

  numWords :: Line -> [(Word, Pos)]
  numWords = flip zip [0..] . cleanWords . words

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

  getIndexedWords :: Index -> [Word] -> [IndexedWord]
  getIndexedWords i = concatMap (\w -> map (\(p, l) -> IndexedWord{word=w,pos=p,line=l}) (item w))
    where item w = case (lookup w i) of
                     Just x -> x
                     Nothing -> (concatMap snd . toList . filterWithKey (\k _ -> isPrefixOf w k)) i

  groupByLine :: [IndexedWord] -> [[IndexedWord]]
  groupByLine = map (sortBy(compare `on` pos)) .
                 groupBy ((==) `on` line) .
                 sortBy(compare `on` line)

  equalTermsAndResults :: [Word] -> [IndexedWord] -> Bool
  equalTermsAndResults ws rs = map word rs == ws

  consecutiveWords :: [IndexedWord] -> Bool
  consecutiveWords rs = all (\(x,y) -> y - x <= 2) (zip ps (drop 1 ps))
        where ps = map pos rs

  toTuple :: IndexedWord -> (Word, Pos, LineNumber)
  toTuple iw = (word iw, pos iw, line iw)

  search :: Index -> String -> [(Word, Pos, LineNumber)]
  search i s = map toTuple (concat (filter predicate (groupByLine plwords)))
        where
              predicate = both (equalTermsAndResults cleaned_words) consecutiveWords
              cleaned_words = (cleanWords . words) s
              plwords = getIndexedWords i cleaned_words
