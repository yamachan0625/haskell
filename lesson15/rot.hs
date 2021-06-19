largestCharNumber :: Int
largestCharNumber = fromEnum (maxBound ::Char)

rootChar :: Char -> Char 
rootChar charToEncrypt = rotN sizeOfAlphabet charToEncrypt
  where sizeOfAlphabet = 1 + fromEnum (maxBound  :: Char)


rotN :: (Bounded a, Enum a) => Int -> a -> a
rotN alphabetSize c = toEnum rotation
  where rotation = offset `mod` alphabetSize -- 剰余演算を使ってEnumの範囲内に収める
        offset = fromEnum c + halfAlphabet -- 中間地からオフセットを特定
        halfAlphabet = alphabetSize `div` 2 -- アルファベットの中間地を特定


rotNdecoder :: (Bounded a, Enum a) => Int -> a -> a
rotNdecoder size val = toEnum rotation
  where rotation = offset `mod` size
        offset = if even size
                 then fromEnum val + halfN
                 else 1 + fromEnum val + halfN
        halfN = size `div` 2

data FourLetterAlphabet = L1 | L2 | L3 | L4 deriving (Show,Enum ,Bounded)

fourLetterMessage :: [FourLetterAlphabet]
fourLetterMessage = [L1,L3,L4,L1,L1,L2]

fourLetterEncoder :: [FourLetterAlphabet] -> [FourLetterAlphabet]
fourLetterEncoder vals = map rot41 vals
  where rot41 = rotN alphaSize
        alphaSize = 1 + fromEnum  (maxBound :: FourLetterAlphabet)


data ThreeLetterAlphabet = Alpha | Beta | Kappa deriving (Show,Enum ,Bounded)

threeLetterMessage :: [ThreeLetterAlphabet]
threeLetterMessage = [Alpha,Alpha,Beta,Alpha,Kappa]



threeLetterEncoder :: [ThreeLetterAlphabet] -> [ThreeLetterAlphabet]
threeLetterEncoder vals = map rot31 vals
  where rot31 = rotN alphaSize
        alphaSize = 1 + fromEnum  (maxBound :: ThreeLetterAlphabet)

threeLetterDecoder :: [ThreeLetterAlphabet] -> [ThreeLetterAlphabet]
threeLetterDecoder vals = map rot31 vals
  where rot31 = rotNdecoder alphaSize
        alphaSize = 1 + fromEnum  (maxBound :: ThreeLetterAlphabet)


rotEncoder :: String -> String
rotEncoder text = map rotChar text
  where rotChar = rotN alphaSize
        alphaSize = 1 + fromEnum  (maxBound::Char)

rotDecoder :: String -> String
rotDecoder text = map rotCharDecoder text
  where rotCharDecoder = rotNdecoder alphaSize
        alphaSize = 1 + fromEnum (maxBound :: Char)
        

class Cipher a where
  encode :: a -> String -> String 
  decode :: a -> String -> String

data Rot = Rot

instance Cipher Rot where
  encode Rot text = rotEncoder text
  decode Rot text = rotDecoder text