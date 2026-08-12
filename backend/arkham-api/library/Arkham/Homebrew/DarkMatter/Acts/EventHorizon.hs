module Arkham.Homebrew.DarkMatter.Acts.EventHorizon (eventHorizon) where

import Arkham.Act.Import.Lifted
import Arkham.Helpers.Agenda
import Arkham.Homebrew.DarkMatter.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.DarkMatter.Sets qualified as Set
import Arkham.Matcher

newtype EventHorizon = EventHorizon ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

eventHorizon :: ActCard EventHorizon
eventHorizon =
  act (1, A) EventHorizon Cards.eventHorizon
    $ Just
    $ GroupClueCost (PerPlayer 2) (locationIs Locations.engineRoomTatterdemalion)

instance RunMessage EventHorizon where
  runMessage msg a@(EventHorizon attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      doom <- getDoomOnAgenda
      shuffleSetAsideEncounterSetIntoEncounterDeck Set.ArtificialIntelligence
      shuffleEncounterDiscardBackIn
      push $ AdvanceAgendaDeck 1 (toSource attrs)
      placeDoomOnAgendaAndCheckAdvance doom
      advanceActDeck attrs
      pure a
    _ -> EventHorizon <$> liftRunMessage msg attrs
