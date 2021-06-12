testAdd [] = []
testAdd (x:xs) = ("a " ++ x):testAdd xs

-- map
myMap f [] = []
myMap f (x:xs) = (f x):myMap f xs
-- myMap (\x -> x + 1) [1,2,3]  => [2,3,4]

