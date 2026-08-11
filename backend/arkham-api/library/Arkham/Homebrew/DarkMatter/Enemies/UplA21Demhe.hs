module Arkham.Homebrew.DarkMatter.Enemies.UplA21Demhe (uplA21Demhe) where

import Arkham.Enemy.Import.Lifted
import Arkham.Homebrew.DarkMatter.CardDefs.Enemies qualified as Cards
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.DarkMatter.Traits (pattern AI)
import Arkham.Matcher

newtype UplA21Demhe = UplA21Demhe EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | "Spawn - Cargo Hold. Hunter. While moving or engaging, UPL-A21 'Demhe'
ignores investigators without an [[AI]] encounter card in their threat area."

The "ignores" clause is modeled as prey: both hunter movement and engagement
consult the prey matcher, so restricting prey to investigators with an AI card
in their threat area reproduces both halves of the rule.
-}
uplA21Demhe :: EnemyCard UplA21Demhe
uplA21Demhe =
  enemy UplA21Demhe Cards.uplA21Demhe
    & setSpawnAt (locationIs Locations.cargoHold)
    & setPrey (HasMatchingTreachery $ TreacheryWithTrait AI)

instance RunMessage UplA21Demhe where
  runMessage msg (UplA21Demhe attrs) = UplA21Demhe <$> runMessage msg attrs
