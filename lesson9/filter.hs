
-- filter
myFilter f [] = []
myFilter f (x:xs) = if f x
                    then x:myFilter f xs
                    else myFilter f xs


remove f [] = []
remove f (x:xs) = if f x
                  then remove f xs
                  else x:remove f xs