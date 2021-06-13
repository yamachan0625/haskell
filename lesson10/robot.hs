robot (name,attack,hp) = \message -> message (name,attack,hp)
killerRobot = robot ("Kill3r",25,200)
gentkeGient = robot ("Mr. Friendly", 10 ,300)

name (n,_,_) = n
attack (_,a,_) = a
hp (_,_,h) = h

getName aRobot =  aRobot name
-- getName killerRobot
-- killerRobot (\(a,b,c) -> a) と同じ
getAttack aRobot = aRobot attack
getHp aRobot = aRobot hp

setName aRobot newName = aRobot (\(n,a,h) -> robot (newName,a,h))
setAttack aRobot newAttack = aRobot (\(n,a,h) -> robot (n,newAttack,h))
setHp aRobot newHp = aRobot (\(n,a,h) -> robot (n,a,newHp))

nicerRobot = setName killerRobot "kitty"
gentlerRobot = setAttack killerRobot 5
softerRobot = setHp killerRobot 50

printRobot aRobot = aRobot (\(n,a,h) -> n ++ " attack:" ++ (show a) ++ "hp:" ++ (show h))

damage aRobot attackDamage = aRobot (\(n,a,h) -> robot (n,a,h - attackDamage))

fight aRobot defender = damage defender attack
  where attack = if getHp aRobot > 10
                 then getAttack aRobot
                 else 0 

gentkeGient1 = fight killerRobot gentkeGient
killerRobot1 = fight gentkeGient killerRobot
gentkeGient2 = fight killerRobot1 gentkeGient1
killerRobot2 = fight gentkeGient1 killerRobot1
gentkeGient3 = fight killerRobot2 gentkeGient2
killerRobot3 = fight gentkeGient2 killerRobot2