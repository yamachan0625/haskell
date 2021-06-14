data FiveSideDie = Side1 | Side2 | Side3 | Side4 | Side5 deriving (Enum,Eq,Show)

-- EqもしくはEnum型クラスに属する任意の型の引数を一つ取る
-- 型 aはEqかEnumクラスのインスタンスである
-- DieクラスはEqクラスとEnumクラスのインスタンスでなければならない
class (Eq a, Enum a) => Die a where
  roll :: Int -> a

-- DieクラスのインスタンスとしてFiveSideDie型を考える
instance Die FiveSideDie where
  roll n = toEnum (n `mod` 5)