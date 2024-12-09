module Arkham.Location.Cards.LodgeGatesMembersOnly (
  lodgeGatesMembersOnly,
  LodgeGatesMembersOnly (..),
)
where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype LodgeGatesMembersOnly = LodgeGatesMembersOnly LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lodgeGatesMembersOnly :: LocationCard LodgeGatesMembersOnly
lodgeGatesMembersOnly = location LodgeGatesMembersOnly Cards.lodgeGatesMembersOnly 2 (PerPlayer 1)

instance HasModifiersFor LodgeGatesMembersOnly where
  getModifiersFor (LodgeGatesMembersOnly a) = whenRevealed a $ modifySelect a AnyEnemy [CannotSpawnIn (be a)]

instance HasAbilities LodgeGatesMembersOnly where
  getAbilities (LodgeGatesMembersOnly attrs) =
    extendRevealed1 attrs
      $ withTooltip "On second thought, maybe coming here was a bad idea" (locationResignAction attrs)

instance RunMessage LodgeGatesMembersOnly where
  runMessage msg (LodgeGatesMembersOnly attrs) = LodgeGatesMembersOnly <$> runMessage msg attrs
