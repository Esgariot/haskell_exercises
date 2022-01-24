

-- [1,2,3] -> [[1],[1,2],[1,2,3]]
prefixes :: [a] -> [[a]]
prefixes = foldr (\x acc -> [x]:map (x :) acc) [] 
-- (a -> b -> b) b (t a)