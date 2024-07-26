module Arkham.Classes.HasAbilities where

import Arkham.Prelude

import Arkham.Ability.Types

class HasAbilities a where
  getAbilities :: a -> [Ability]
  getAbilities = const []

instance HasAbilities a => HasAbilities (With a b) where
  getAbilities (With a _) = getAbilities a
