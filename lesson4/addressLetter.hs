addressLetter name location = nameText ++ " - " ++ location
 where nameText = fst name ++ " " ++ snd name

sfOffice name = if lastName < "L"
                then nameText ++ " -PO Box 1234 -san"
                else nameText ++ " -PO Box 5678 -san"
  where lastName = snd name
        nameText = fst name ++ " " ++ snd name

nyOffice name = nameText ++ ": PO Box 789"
 where nameText = fst name ++ " " ++ snd name

renoOffice name = nameText ++ " - PO Box 456"
 where nameText = snd name

wdcOffice name = nameText ++ " - Esq"
 where nameText = fst name ++ " " ++ snd name

getLocationFunction location = case location of -- locationの値を調べるcase
 -- locationがnyの場合はnyOfficeを返す
 "ny" -> nyOffice
 -- locationがsfの場合はsfOfficeを返す
 "sf" -> sfOffice
  -- locationがrenoの場合はrenoOfficeを返す
 "reno" -> renoOffice
 "wdc" -> wdcOffice
 -- その他の場合は汎用的な解を返す (_ はワイルドカード)
 _ -> (\name -> (fst name) ++ " " ++ (snd name))


addressLetter2 name location = locationFunction name
 where locationFunction =  getLocationFunction location