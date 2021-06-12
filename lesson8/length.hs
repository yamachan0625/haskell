-- lengthを調べる
myLength [] = 0
myLength xs = 1 + myLength(tail xs)

-- パターンマッチングver
myLength2 [] = 0
myLength2 (x:xs) = 1 + myLength2 xs

