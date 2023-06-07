module Arkham.Location.Cards.LodgeGatesMembersOnly
  ( lodgeGatesMembersOnly
  , LodgeGatesMembersOnly(..)
  )
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype LodgeGatesMembersOnly = LodgeGatesMembersOnly LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lodgeGatesMembersOnly :: LocationCard LodgeGatesMembersOnly
lodgeGatesMembersOnly = location LodgeGatesMembersOnly Cards.lodgeGatesMembersOnly 2 (PerPlayer 1)

instance HasAbilities LodgeGatesMembersOnly where
  getAbilities (LodgeGatesMembersOnly attrs) =
    getAbilities attrs
    -- withRevealedAbilities attrs []

instance RunMessage LodgeGatesMembersOnly where
  runMessage msg (LodgeGatesMembersOnly attrs) =
    LodgeGatesMembersOnly <$> runMessage msg attrs
