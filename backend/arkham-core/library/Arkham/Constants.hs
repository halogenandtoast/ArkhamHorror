module Arkham.Constants where

import Arkham.Prelude

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

pattern ActAdvancement :: Int
pattern ActAdvancement <- 999
  where
    ActAdvancement = 999

pattern ResignAbility :: Int
pattern ResignAbility <- 99
  where
    ResignAbility = 99

pattern AbilityInvestigate :: Int
pattern AbilityInvestigate <- 101
  where
    AbilityInvestigate = 101
