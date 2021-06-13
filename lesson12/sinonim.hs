type FirstName = String 
type LastName = String
type Age = Int
type Height = Int 
data Sex = Male | Female

patientInfo :: FirstName -> LastName -> Age -> Height -> String
patientInfo fname lname age height = fname ++ lname ++ show age ++ show height

type PatientName = (String,String)
firstname :: PatientName -> String
firstname patient = fst patient

lastName ::PatientName -> String
lastName patient = snd patient

patientInfo2 :: PatientName -> Age -> Height -> String
patientInfo2 patient age height = firstname patient ++ lastName patient ++ show age ++ show height

patientInfo3 :: PatientName -> Age -> Height -> String
patientInfo3 (fname,lname) age height = fname++ lname ++ show age ++ show height

data RhType = Pos | Neg
data ABOType = A | B | AB | O
data BloodType = BloodType ABOType RhType

patient1BT :: BloodType
patient1BT = BloodType A Pos

patient2BT :: BloodType
patient2BT = BloodType O Neg

patient3BT :: BloodType
patient3BT = BloodType AB Pos

showRh :: RhType -> String 
showRh Pos = "+"
showRh Neg = "-"

showABO :: ABOType -> String 
showABO A = "A"
showABO B = "B"
showABO AB = "AB"
showABO O = "O"

showBloodType :: BloodType -> String 
showBloodType (BloodType abo rh) = showABO abo ++ showRh rh

canDonateTo :: BloodType -> BloodType -> Bool 
canDonateTo (BloodType O _) _ = True --どの血液型にも輸血できる
canDonateTo _ (BloodType AB _) = True　--どの血液型からでも輸血できる
canDonateTo (BloodType A _) (BloodType A _) = True
canDonateTo (BloodType B _) (BloodType B _) = True
canDonateTo _ _ = False --上記のどれにも該当しない場合

type MiddleName = String
data Name = NormalName FirstName LastName | NameWithMiddle FirstName MiddleName LastName

showName :: Name -> String
showName (NormalName f l) = f ++ " " ++ l
showName (NameWithMiddle f m l) = f ++ " " ++ m ++ " " ++ l

name1 :: Name
name1 = NormalName "Jerome" "Salinger"
name2 :: Name
name2 = NameWithMiddle "Jerome" "David" "Salinger"



data Patient = Patient Name Sex Int Int Int BloodType
johnDoe = Patient (NormalName "John" "Doe") Male 30 74 200 (BloodType AB Pos)

data PatientRecord = PatientRecord {name:: Name, sex::Sex, age::Int, height::Int, weight::Int, bloodType:: BloodType}

jackieSmith :: PatientRecord
jackieSmith = PatientRecord {name = NormalName "Jackie" "Smith",age = 43, sex = Female, height = 62, weight = 115, bloodType = BloodType O Neg}


canDonateToLesson :: PatientRecord -> PatientRecord -> Bool
canDonateToLesson p1 p2 = canDonateTo (bloodType p1) (bloodType p2)