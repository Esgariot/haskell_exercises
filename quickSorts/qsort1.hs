myQsort :: Integral a => [a] -> [a]
myQsort [] = []
myQsort (x : xs) = lesser ++ [x] ++ greaterEq
  where
    lesser = myQsort . filter (< x) $ xs
    greaterEq = myQsort . filter (>= x) $ xs
