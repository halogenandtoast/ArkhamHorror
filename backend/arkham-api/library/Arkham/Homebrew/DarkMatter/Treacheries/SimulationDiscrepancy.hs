module Arkham.Homebrew.DarkMatter.Treacheries.SimulationDiscrepancy (simulationDiscrepancy) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), inThreatAreaGets)
import Arkham.Homebrew.DarkMatter.CardDefs.Treacheries qualified as Cards
import Arkham.Matcher
import Arkham.Treachery.Import.Lifted hiding (DiscoverClues)

newtype SimulationDiscrepancy = SimulationDiscrepancy TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

simulationDiscrepancy :: TreacheryCard SimulationDiscrepancy
simulationDiscrepancy = treachery SimulationDiscrepancy Cards.simulationDiscrepancy

{- | "Forced - When you would discover any amount of clues: Discover 1 fewer clue,
take 1 horror, and discard Simulation Discrepancy instead."
-}
instance HasModifiersFor SimulationDiscrepancy where
  getModifiersFor (SimulationDiscrepancy a) = inThreatAreaGets a [DiscoveredClues (-1)]

instance HasAbilities SimulationDiscrepancy where
  getAbilities (SimulationDiscrepancy a) =
    [mkAbility a 1 $ forced $ DiscoverClues #when You Anywhere AnyValue]

instance RunMessage SimulationDiscrepancy where
  runMessage msg t@(SimulationDiscrepancy attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignHorror iid attrs 1
      toDiscardBy iid attrs attrs
      pure t
    _ -> SimulationDiscrepancy <$> liftRunMessage msg attrs
