import Control.Monad.State

myQsortState :: Ord a => [a] -> State Int [a]
myQsortState [] =
  return []
myQsortState (x : xs) = do
  l <- lesser
  ge <- greaterEqual
  modify (+ 1)
  return $ l ++ [x] ++ ge
  where
    lesser = myQsortState . filter (< x) $ xs
    greaterEqual = myQsortState . filter (>= x) $ xs

myQsort xs = runState (myQsortState xs) 0