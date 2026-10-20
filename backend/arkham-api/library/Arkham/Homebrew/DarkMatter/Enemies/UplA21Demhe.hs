module Arkham.Homebrew.DarkMatter.Enemies.UplA21Demhe (uplA21Demhe) where

import Arkham.Enemy.Import.Lifted
import Arkham.Homebrew.DarkMatter.CardDefs.Enemies qualified as Cards
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.DarkMatter.Traits (pattern AI)
import Arkham.Matcher

newtype UplA21Demhe = UplA21Demhe EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

uplA21Demhe :: EnemyCard UplA21Demhe
uplA21Demhe =
  enemy UplA21Demhe Cards.uplA21Demhe
    & setSpawnAt (locationIs Locations.cargoHold)
    & setOnlyPrey (HasMatchingTreachery $ TreacheryWithTrait AI)

instance RunMessage UplA21Demhe where
  runMessage msg (UplA21Demhe attrs) = UplA21Demhe <$> runMessage msg attrs
