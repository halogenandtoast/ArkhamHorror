module Arkham.Classes.HasAbilities where

import Arkham.Prelude
import Arkham.Ability.Types

class HasAbilities a where
  getAbilities :: a -> [Ability]
  getAbilities = const []

instance HasAbilities a => HasAbilities (With a b) where
  getAbilities (With a _) = getAbilities a

withBaseAbilities :: HasAbilities a => a -> [Ability] -> [Ability]
withBaseAbilities a f = getAbilities a <> f

extend :: HasAbilities a => a -> [Ability] -> [Ability]
extend = withBaseAbilities

extend1 :: HasAbilities a => a -> Ability -> [Ability]
extend1 a ab = withBaseAbilities a [ab]
