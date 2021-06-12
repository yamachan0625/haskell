ifEven f x = if even x
             then f x
             else x

getIfEven f = (\x -> ifEven f x)
test = getIfEven (\y -> y + y)
genIfXEven x = (\f -> ifEven f x)
test2 = genIfXEven 8
aaa = test2(\y -> y + 5)