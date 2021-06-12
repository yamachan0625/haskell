myReverse [] = []
myReverse (x:[]) = [x]
myReverse (x:xs) = myReverse xs ++ [x]
-- myReverse [1,2,3] = myReverse [2,3] ++ [1]
-- myReverse [2,3] = myReverse [3] ++ [2] ++ [1]
-- myReverse [3] = [3] ++ [2] ++ [1]
-- [3,2,1]




aaa (a:xs) = xs ++ [a]