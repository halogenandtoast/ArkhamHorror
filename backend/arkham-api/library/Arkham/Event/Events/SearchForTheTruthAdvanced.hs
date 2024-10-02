module Arkham.Event.Events.SearchForTheTruthAdvanced where

import Arkham.Capability
import Arkham.Card
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Investigator.Types (Field (..))
import Arkham.Projection

newtype SearchForTheTruthAdvanced = SearchForTheTruthAdvanced EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

searchForTheTruthAdvanced :: EventCard SearchForTheTruthAdvanced
searchForTheTruthAdvanced = event SearchForTheTruthAdvanced Cards.searchForTheTruthAdvanced

instance RunMessage SearchForTheTruthAdvanced where
  runMessage msg e@(SearchForTheTruthAdvanced attrs) = runQueueT $ case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      x <- fieldMap InvestigatorClues (min 5) iid
      doStep x msg
      pure e
    DoStep x msg'@(PlayThisEvent iid eid) | eid == toId attrs && x > 0 -> do
      discardPile <- field InvestigatorDiscard iid
      mLocation <- field InvestigatorLocation iid
      canDrawCards <- can.draw.cards iid
      when (isJust mLocation || canDrawCards) do
        chooseOrRunOneM iid do
          when canDrawCards do
            labeled "Draw 1 Card" do
              drawCardsIfCan iid attrs 1
              doStep (x - 1) msg'
          labeled "Place that clue on your location and return any card from your discard pile to your hand" do
            push $ InvestigatorPlaceCluesOnLocation iid (toSource attrs) 1
            chooseOneM iid do
              for_ discardPile \card ->
                targeting card $ addToHand iid [toCard card]
            doStep (x - 1) msg'
          labeled "Do nothing (finishes this card)" nothing

      pure e
    _ -> SearchForTheTruthAdvanced <$> liftRunMessage msg attrs
