module Arkham.Location.Cards.LodgeGatesMembersOnly (lodgeGatesMembersOnly) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.ForTheGreaterGood.Helpers

newtype LodgeGatesMembersOnly = LodgeGatesMembersOnly LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lodgeGatesMembersOnly :: LocationCard LodgeGatesMembersOnly
lodgeGatesMembersOnly = location LodgeGatesMembersOnly Cards.lodgeGatesMembersOnly 2 (PerPlayer 1)

instance HasModifiersFor LodgeGatesMembersOnly where
  getModifiersFor (LodgeGatesMembersOnly a) = whenRevealed a $ modifySelect a AnyEnemy [CannotSpawnIn (be a)]

instance HasAbilities LodgeGatesMembersOnly where
  getAbilities (LodgeGatesMembersOnly a) =
    extendRevealed1 a
      $ scenarioI18n
      $ withI18nTooltip "lodgeGatesMembersOnly.resign" (locationResignAction a)

instance RunMessage LodgeGatesMembersOnly where
  runMessage msg (LodgeGatesMembersOnly attrs) = LodgeGatesMembersOnly <$> runMessage msg attrs
