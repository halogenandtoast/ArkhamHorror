module Arkham.Homebrew.DarkMatter.Treacheries.SimulationDiscrepancy (simulationDiscrepancy) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Helpers.Window (getDiscover)
import Arkham.Homebrew.DarkMatter.CardDefs.Treacheries qualified as Cards
import Arkham.Matcher
import Arkham.Treachery.Import.Lifted

newtype SimulationDiscrepancy = SimulationDiscrepancy TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

simulationDiscrepancy :: TreacheryCard SimulationDiscrepancy
simulationDiscrepancy = treachery SimulationDiscrepancy Cards.simulationDiscrepancy

{- | "Forced - When you would discover any amount of clues: Discover 1 fewer clue,
take 1 horror, and discard Simulation Discrepancy instead."
-}
instance HasAbilities SimulationDiscrepancy where
  getAbilities (SimulationDiscrepancy a) =
    [restricted a 1 (InThreatAreaOf You) $ forced $ WouldDiscoverClues #when You Anywhere (atLeast 1)]

instance RunMessage SimulationDiscrepancy where
  runMessage msg t@(SimulationDiscrepancy attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    {- The reduction has to be scoped to this discovery, not to the treachery: the
    discard below removes the card before the amount is recomputed, and an
    investigator-wide 'DiscoveredClues' only applies to investigate discoveries. -}
    UseCardAbility iid (isSource attrs -> True) 1 (getDiscover -> did) _ -> do
      roundModifier attrs (DiscoverTarget did) (DiscoveredClues (-1))
      assignHorror iid attrs 1
      toDiscardBy iid attrs attrs
      pure t
    _ -> SimulationDiscrepancy <$> liftRunMessage msg attrs
