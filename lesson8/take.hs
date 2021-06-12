-- からの配列から何個取り出しても空
myTake _ [] = []
-- 配列から0個取り出す = 空
myTake 0 _ = []
myTake n (x:xs) = x:rest
  where rest = myTake (n - 1) xs

-- myTake 3 [1,2,3,4,5]
-- myTake 2 [2,3,4,5]
-- myTake 1 [3,4,5]
-- myTake 0 [4,5]
-- 1:2:3:[] = [1,2,3]