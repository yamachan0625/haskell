import Data.List

names = [("An", "Zurtis"), ("Bernade","Yumer"), ("Ceter","Xook"),("Deter","Xook"),("Dtephen","Worriss")]

compareLastNames :: Ord a1 => (a2, a1) -> (a3, a1) -> Ordering
compareLastNames name1 name2 = if lastName1 > lastName2
                              then GT 
                              else if lastName1 < lastName2
                                then LT 
                                else EQ 

  where lastName1 = snd name1
        lastName2 = snd name2


compareLastNames2 name1 name2 = if lastName1 > lastName2 -- lastName大きければ
                              then GT 
                              else if lastName1 < lastName2 -- lastName小さければ
                                then LT
                                else if firstname1 > firstname2 -- lastnameが等しくfirstnameが大きければ
                                  then GT 
                                  else if firstname1 < firstname2 -- firstnameが大きければ
                                    then LT 
                                    else EQ -- firstnameが等しければ

  where lastName1 = snd name1
        lastName2 = snd name2
        firstname1 = fst name1
        firstname2 = fst name2

compareLastNames3 name1 name2
  | lastName1 > lastName2 = GT
  | lastName1 < lastName2 = LT
  | firstname1 > firstname2 = GT
  | firstname1 < firstname2 = LT
  | otherwise = EQ
  where
      lastName1 = snd name1
      lastName2 = snd name2
      firstname1 = fst name1
      firstname2 = fst name2