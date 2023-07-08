module Arkham.Location.Cards.LodgeGatesMembersOnly (
  lodgeGatesMembersOnly,
  LodgeGatesMembersOnly (..),
)
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher

newtype LodgeGatesMembersOnly = LodgeGatesMembersOnly LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lodgeGatesMembersOnly :: LocationCard LodgeGatesMembersOnly
lodgeGatesMembersOnly = location LodgeGatesMembersOnly Cards.lodgeGatesMembersOnly 2 (PerPlayer 1)

instance HasModifiersFor LodgeGatesMembersOnly where
  getModifiersFor (EnemyTarget _) (LodgeGatesMembersOnly attrs) =
    pure $ toModifiers attrs [CannotSpawnIn (LocationWithId $ toId attrs)]
  getModifiersFor _ _ = pure []

instance HasAbilities LodgeGatesMembersOnly where
  getAbilities (LodgeGatesMembersOnly attrs) =
    withRevealedAbilities
      attrs
      [withTooltip "On second thought, maybe coming here was a bad idea" (locationResignAction attrs)]

instance RunMessage LodgeGatesMembersOnly where
  runMessage msg (LodgeGatesMembersOnly attrs) =
    LodgeGatesMembersOnly <$> runMessage msg attrs
