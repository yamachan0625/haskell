import Data.Char
-- 文字列の比較はシングルkウォーとでなければならない
testFilter text = filter (/= ' ') text

isPalindrome text = lowerText == reverse lowerText
  where lowerText = map toLower noSpace
        noSpace = filter (/= ' ') text