printDouble :: Int -> String
printDouble i = show (i * 2)

-- filter型
-- filter :: (a -> Bool ) -> [a] -> [a]

safeTail :: [a] -> [a]
safeTail [] = []
safeTail (a:xs) = xs


