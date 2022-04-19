-- create a cup constructor
type Prop = Float -> Float
type CupObj = Prop -> Float

cup :: Float -> CupObj
cup ml = \f -> f ml


--create a properties
getMl :: CupObj -> Float
getMl aCup = aCup id

drinkAll :: CupObj -> CupObj
drinkAll aCup = cup 0

drink :: CupObj -> Float -> CupObj
drink aCup toDrink
    | toDrink <= toSee = cup (toSee - toDrink)
    | otherwise = cup 0
    where toSee = getMl aCup
