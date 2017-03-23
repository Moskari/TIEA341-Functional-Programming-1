module Exercise where
import Prelude hiding (reverse)

deleteLast :: Eq a => a -> [a] -> [a]
deleteLast _ [] = []

deleteLast d [x] = if x == d then [] else [x]
deleteLast d l = let
  findLastInd list index lindex =
    case list of
      (y:ys) -> findLastInd ys (index+1) (if y == d then Just index else lindex)
      [] -> lindex
  lastIndex = findLastInd l 0 Nothing
  removeInd i li = case i of
    Just i -> take i li ++ drop (i+1) li
    Nothing -> li
  in removeInd lastIndex l --take lastIndex l ++ drop (lastIndex+1) l


-- deleteLast 1 [1,2,1,1,2,3] == [1,2,1,2,3]