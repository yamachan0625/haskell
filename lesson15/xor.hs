xorBool :: Bool -> Bool -> Bool 
xorBool value1 value2 = (value1 || value2) && (not (value1 && value2))

xorPair :: (Bool,Bool) -> Bool 
xorPair (v1,v2) = xorBool v1 v2

xor :: [Bool] -> [Bool] -> [Bool]
xor list1 list2 = map xorPair (zip list1 list2)



type Bits = [Bool]

intToBits' :: Int -> Bits
intToBits' 0 = [False]
intToBits' 1 = [True]
intToBits' n = if remainder == 0
               then False : intToBits' nextVal
               else True : intToBits' nextVal
  where remainder = n `mod` 2
        nextVal = n `div` 2


maxBits :: Int 
maxBits = length (intToBits' maxBound)

-- 余分なFalseを追加してリストの長さをIntのmaxBound値を返還した時のリストの長さ(maxBits)と同じにする
intToBits :: Int -> Bits
intToBits n = leadingFalses ++ reversedBits
  where leadingFalses :: [Bool]
        leadingFalses = take missingBits (cycle [False])
        missingBits :: Int
        missingBits = maxBits - (length reversedBits)
        reversedBits :: [Bool]
        reversedBits = reverse (intToBits' n)

charToBits :: Char -> Bits
charToBits char = intToBits (fromEnum  char)

bitsToInt ::Bits -> Int 
bitsToInt bits = sum (map (\x -> 2^(snd x)) trueLocations)
  where size = length bits
        indices :: [Int]
        indices = [size -1, size-2 .. 0]
        trueLocations :: [(Bool, Int)]
        trueLocations = filter (\x -> fst x == True) (zip bits indices)

bitsToChar :: Bits -> Char 
bitsToChar bits = toEnum (bitsToInt bits)



myPad :: String 
myPad = "Shhhhhh"

myPlainText :: String 
myPlainText = "Haskell"

applyOTP' :: String -> String -> [Bits]
applyOTP' pad plaintext = map (\pair -> (fst pair) `xor` (snd pair)) (zip padBits plaintextBits)
  where padBits = map charToBits pad
        plaintextBits =  map charToBits plaintext

applyOTP :: String -> String -> String 
applyOTP pad plainText = map bitsToChar bitList
  where bitList = applyOTP' pad plainText

encoderDecoder :: String -> String 
encoderDecoder = applyOTP myPad

class Cipher a where
  encode :: a -> String -> String 
  decode :: a -> String -> String

data OneTimePad = OTP String 

instance Cipher OneTimePad where
  encode (OTP pad) text = applyOTP pad text
  decode (OTP pad) text = applyOTP pad text