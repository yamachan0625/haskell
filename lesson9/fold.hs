-- リストに含まれる文字列を全て連結する
concatAll xs = foldl (++) "" xs

-- リストの各要素を２乗した上で総和を求める
sumOfSquares xs = foldl (+) 0 (map (^2) xs)

-- リストが空になったらinitを返す
myFoldl f init [] = init
myFoldl f init (x:xs) = myFoldl f newInit xs
  where newInit = f init x

myFoldr f init [] = init 
myFoldr f init (x:xs) = f x rightResult
  where rightResult = myFoldr f init xs
-- myFoldr (+) 0 [1,2,3,4]
-- 1 + (myFoldr (+) 0 [2,3,4])
-- 1 + (2 + (myFoldr (+) 0 [3,4]))
-- 1 + (2 + (3 + (myFoldr (+) 0 [4])))
-- 1 + (2 + (3 + (4 + (myFoldr (+) 0 []))))