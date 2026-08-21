module Arkham.Location.Cards.ChildrenOfBlood.RiverOfBlood.GarrisonStreetDusk (garrisonStreetDusk) where

import Arkham.Helpers.Modifiers (ModifierType (..), modifySelfWhenM)
import Arkham.Location.CardDefs.ChildrenOfBlood.RiverOfBlood qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype GarrisonStreetDusk = GarrisonStreetDusk LocationAttrs
  deriving anyclass (IsLocation, RunMessage)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

garrisonStreetDusk :: LocationCard GarrisonStreetDusk
garrisonStreetDusk = symbolLabel $ location GarrisonStreetDusk Cards.garrisonStreetDusk 3 (PerPlayer 1)

instance HasModifiersFor GarrisonStreetDusk where
  getModifiersFor (GarrisonStreetDusk a) =
    modifySelfWhenM a (selectAny $ enemyAt a) [ShroudModifier 1]
