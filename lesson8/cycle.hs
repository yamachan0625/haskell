finiteCycle (first:rest) = first:rest ++ [first]
myCycle (first:rest) = first:myCycle (rest ++ [first])
-- myCycle [1,2,3] = 1:myCycle [2,3,1]
-- myCycle [1,2,3] = 1: 2: [myCycle [3,1,2]
