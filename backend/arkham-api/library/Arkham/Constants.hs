module Arkham.Constants where

import Arkham.Prelude

pattern ResignAbility :: Int
pattern ResignAbility <- 99
  where
    ResignAbility = 99

pattern AbilityAttack :: Int
pattern AbilityAttack <- 100
  where
    AbilityAttack = 100

pattern AbilityEvade :: Int
pattern AbilityEvade <- 101
  where
    AbilityEvade = 101

pattern AbilityEngage :: Int
pattern AbilityEngage <- 102
  where
    AbilityEngage = 102

pattern AbilityInvestigate :: Int
pattern AbilityInvestigate <- 103
  where
    AbilityInvestigate = 103

pattern AbilityMove :: Int
pattern AbilityMove <- 104
  where
    AbilityMove = 104

pattern VeiledAbility :: Int
pattern VeiledAbility <- 199
  where
    VeiledAbility = 199

pattern MirageAbility :: Int
pattern MirageAbility <- 200
  where
    MirageAbility = 200

pattern ActAdvancement :: Int
pattern ActAdvancement <- 999
  where
    ActAdvancement = 999

pattern PlayAbility :: Int
pattern PlayAbility <- 1000
  where
    PlayAbility = 1000

pattern ResourceAbility :: Int
pattern ResourceAbility <- 1001
  where
    ResourceAbility = 1001

pattern VehicleEnterExitAbility :: Int
pattern VehicleEnterExitAbility <- 2000
  where
    VehicleEnterExitAbility = 2000

pattern NonActivateAbility :: Int
pattern NonActivateAbility <- 2001
  where
    NonActivateAbility = 2001

notPlayerAbilityIndex :: Int -> Bool
notPlayerAbilityIndex n = n `notElem` [AbilityInvestigate, AbilityEvade, AbilityEngage, AbilityMove, AbilityAttack]
