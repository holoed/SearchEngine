module InvertedIndex where

  import Prelude hiding (lookup)
  import Data.Function
  import Data.Char
  import Data.List hiding (insert, lookup)
  import Data.Map (Map, fromList, lookup, elems, insert, keys, empty, (!))
  import Control.Applicative hiding (empty)
  import Data.Maybe (fromMaybe)
  import qualified Data.Set as Set

  type Doc = String
  type Line = String
  type Word = String
  type Pos = Int
  type LineNumber = Int 
  type Index = Map Word [(Pos, LineNumber)]

  data IndexedWord = IndexedWord { word:: Word, pos:: Pos, line:: LineNumber }

  data PartialTermIndex = D (Map Char ([String], PartialTermIndex))

  both :: (a -> Bool) -> (a -> Bool) -> a -> Bool
  both = liftA2 (&&)

  numLines :: Doc -> [(Line, LineNumber)]
  numLines = flip zip [0..] . lines

  numWords :: Line -> [(Word, Pos)]
  numWords = flip zip [0..] . cleanWords . words

  cleanWords :: [Word] -> [Word]
  cleanWords = map (map toLower) .
               filter (not . null) .
               map (\w -> if (isNumber(head w))
                          then w
                          else filter (both isLetter isAscii) w)

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

  createTermIndex :: Doc -> Index
  createTermIndex =  fromList . accumulate . makeLists . sort . allNumWords . numLines

  createPartialTermIndex :: [String] -> PartialTermIndex
  createPartialTermIndex []  = D empty
  createPartialTermIndex ws  =
      foldl (\(D m) (k, ws') -> D $ insert k (ws', createPartialTermIndex (fmap (drop 1) ws')) m) (D empty) .
      fmap (\ws' -> (head . head $ ws', ws')) .
      groupBy ((==) `on` head) .
      sortBy (compare `on` head) .
      filter (not . null) $ ws

  searchTermsFromPartial :: String -> PartialTermIndex -> (Bool, [String])
  searchTermsFromPartial s d = findTerms s d
    where
      flattenValues = fmap (s ++) . concatMap fst . elems

      findTerms [] (D d') = (True, flattenValues d')
      findTerms (x:xs) (D d') =
        case lookup x d' of
              Just (_, d'') -> findTerms xs d''
              Nothing -> (False, flattenValues d')

  createIndex :: Doc -> (Index, PartialTermIndex)
  createIndex doc = (index, createPartialTermIndex (keys index))
    where index = createTermIndex doc

  getWordIndex :: (Index, PartialTermIndex) -> String -> [(Pos, LineNumber)]
  getWordIndex (i, pti) w = fromMaybe partialResults
                                      (fmap (++ partialResults) (lookup w i) )
                where matches = searchTermsFromPartial w pti
                      partialResults = if fst matches
                                       then concatMap (i!) (snd matches)
                                       else []

  getIndexedWords :: (Index, PartialTermIndex) -> [Word] -> [IndexedWord]
  getIndexedWords i = concatMap (\w -> map (\(p, l) -> IndexedWord{word=w,pos=p,line=l}) (getWordIndex i w))

  groupByLine :: [IndexedWord] -> [[IndexedWord]]
  groupByLine = map (sortBy(compare `on` pos)) .
                 groupBy ((==) `on` line) .
                 sortBy(compare `on` line)

  equalTermsAndResults :: [Word] -> [IndexedWord] -> Bool
  equalTermsAndResults ws rs = Set.isSubsetOf (Set.fromList ws) (Set.fromList $ map word rs)

  toTuple :: IndexedWord -> (Word, Pos, LineNumber)
  toTuple iw = (word iw, pos iw, line iw)

  search :: (Index, PartialTermIndex) -> String -> [(Word, Pos, LineNumber)]
  search i s = map toTuple (concat (filter predicate (groupByLine plwords)))
        where
              predicate = equalTermsAndResults cleaned_words
              cleaned_words = (cleanWords . words) s
              plwords = getIndexedWords i cleaned_words
