cup floz = \message -> message floz

coffeeCup = cup 12
getOz aCup = aCup (\floz -> floz)

drink aCup ozDrank = if ozDiff >= 0
                     then cup (floz - ozDrank)
                     else cup 0
  where floz = getOz aCup
        ozDiff = floz - ozDrank

isEmpty aCup = getOz aCup == 0

afterManySips = foldl drink coffeeCup [1,1,1,1,1]