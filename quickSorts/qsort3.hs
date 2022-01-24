import Control.Monad.Trans (lift)
import Control.Monad.Trans.State.Lazy

myQsortStateDo :: (Ord a, Show a) => [a] -> StateT Int IO [a]
myQsortStateDo [] =
  return []
myQsortStateDo (x : xs) = do
  l <- lesser
  ge <- greaterEqual
  modify (+ 1)
  s <- get
  lift . putStrLn $ "Run " ++ show s ++ ": " ++ show l ++ " --> " ++ show x ++ " <-- " ++ show ge
  return $ l ++ [x] ++ ge
  where
    lesser =  myQsortStateDo . filter (< x) $ xs
    greaterEqual = myQsortStateDo . filter (>= x) $ xs

myQsort xs = runStateT (myQsortStateDo xs) 0