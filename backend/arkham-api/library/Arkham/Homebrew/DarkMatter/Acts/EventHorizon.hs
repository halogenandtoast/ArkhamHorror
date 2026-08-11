module Arkham.Homebrew.DarkMatter.Acts.EventHorizon (eventHorizon) where

import Arkham.Act.Import.Lifted
import Arkham.Agenda.Types (Field (AgendaDoom))
import Arkham.Homebrew.DarkMatter.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.DarkMatter.Sets qualified as Set
import Arkham.Matcher
import Arkham.Message (CanAdvance (..))
import Arkham.Projection

newtype EventHorizon = EventHorizon ActAttrs
  deriving anyclass (IsAct, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

eventHorizon :: ActCard EventHorizon
eventHorizon =
  act (1, A) EventHorizon Cards.eventHorizon
    $ Just
    $ GroupClueCost (PerPlayer 2) (locationIs Locations.engineRoomTatterdemalion)

instance RunMessage EventHorizon where
  runMessage msg a@(EventHorizon attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      shuffleSetAsideEncounterSetIntoEncounterDeck Set.ArtificialIntelligence
      shuffleEncounterDiscardBackIn
      {- "Advance to agenda 2a and act 2a. Do not remove doom from play and
      transfer all doom from agenda 1a to agenda 2a." AdvanceAgendaDeck only
      replaces the agenda card, so unlike a normal agenda advance it sweeps no
      doom off anything; we only have to carry agenda 1a's own doom across. -}
      doom <- fromMaybe 0 <$> (traverse (field AgendaDoom) =<< selectOne AnyAgenda)
      push $ AdvanceAgendaDeck 0 (toSource attrs)
      push $ PlaceDoomOnAgenda doom CanAdvance
      advanceActDeck attrs
      pure a
    _ -> EventHorizon <$> liftRunMessage msg attrs
