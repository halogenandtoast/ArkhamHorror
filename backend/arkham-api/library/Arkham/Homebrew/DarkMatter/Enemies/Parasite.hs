module Arkham.Homebrew.DarkMatter.Enemies.Parasite (parasite) where

import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Homebrew.DarkMatter.CardDefs.Enemies qualified as Cards
import Arkham.Matcher

newtype Parasite = Parasite EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

parasite :: EnemyCard Parasite
parasite = enemy Parasite Cards.parasite

{- | "Cannot be disengaged." 'CannotBeDisengagedBy' covers effects that pick a
disengageable enemy; 'DoNotDisengageEvaded' keeps it engaged when it is evaded
(it still exhausts).
-}
instance HasModifiersFor Parasite where
  getModifiersFor (Parasite a) = modifySelf a [CannotBeDisengagedBy AnySource, DoNotDisengageEvaded]

instance RunMessage Parasite where
  runMessage msg (Parasite attrs) = Parasite <$> runMessage msg attrs
