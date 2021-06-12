-- parseに！が含まれているか
respond phrase = if '!' `elem` phrase
                then "wow!"
                else "uh..."

-- リストの後ろから指定した数だけ返す
takeLast n aList = reverse (take n (reverse aList))

assignToGroups :: (Num a, Enum a) => a -> [b] -> [(a, b)]
assignToGroups n aList = zip groups aList
  where groups = cycle [1 .. n]
-- assignToGroups 3 ["a", "b", "c", "d"]
-- [(1,"a"),(2,"b"),(3,"c"),(1,"d")]

-- subweq 開始位置　終了位置 リスト
-- 開始位置と終了位置の間を返す
subseq start end list = take diff (drop start list)
  where diff = end - start


-- 要素がリストの前半部分に含まれるtrue そうでない場合false
inFirstHalf val myList  = val `elem` firstHalf
  where firstHalf = take midpoint myList
        midpoint = (length myList) `div` 2