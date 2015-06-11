module SearchAsYouType where

  import Data.Function
  import Data.List hiding (insert, lookup)
  import Data.Map hiding (filter, foldl, null)
  import Prelude hiding (lookup)

  data Dict = Dict (Map Char ([String], Dict)) deriving Show


  createPartialTermIndex :: [String] -> Dict
  createPartialTermIndex []  = Dict empty
  createPartialTermIndex ws  =
              foldl (\(Dict m) (k, ws') -> Dict $ insert k (ws', createPartialTermIndex (fmap (drop 1) ws')) m) (Dict empty) .
              fmap (\ws' -> (head . head $ ws', ws')) .
              groupBy ((==) `on` head) .
              sortBy (compare `on` head) .
              filter (not . null) $ ws

  findPartialTerm :: String -> Dict -> (Bool, [String])
  findPartialTerm s d = findTerms s d
    where
      findTerms [] (Dict d') = (True, fmap (s ++) $ concatMap fst $ elems d')
      findTerms (x:xs) (Dict d') =
        case lookup x d' of
              Just (_, d'') -> findTerms xs d''
              Nothing -> (False, fmap (s ++) $ concatMap fst $ elems d')
