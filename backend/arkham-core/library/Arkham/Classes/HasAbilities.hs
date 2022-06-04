module Arkham.Classes.HasAbilities where

import Arkham.Prelude

import Arkham.Ability

class HasAbilities a where
  getAbilities :: a -> [Ability]
  getAbilities = const []
