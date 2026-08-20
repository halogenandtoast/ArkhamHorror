module Arkham.Location.Cards.TheFeastOfHemlockVale.HorrorsInTheRock.OvergrownTunnel (overgrownTunnel) where

import Arkham.Cost
import Arkham.Helpers.Modifiers
import Arkham.Location.CardDefs.TheFeastOfHemlockVale.HorrorsInTheRock qualified as Cards
import Arkham.Location.Import.Lifted

newtype OvergrownTunnel = OvergrownTunnel LocationAttrs
  deriving anyclass (IsLocation, RunMessage)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

overgrownTunnel :: LocationCard OvergrownTunnel
overgrownTunnel = locationWith OvergrownTunnel Cards.overgrownTunnel 3 (PerPlayer 1) connectsToAdjacent

instance HasModifiersFor OvergrownTunnel where
  getModifiersFor (OvergrownTunnel a) = do
    modifySelfWhen a (a.clues > 0) [AdditionalCostToLeave $ ActionCost 1]
