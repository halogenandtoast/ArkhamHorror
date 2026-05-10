module Arkham.Event.Events.SearchForTheTruthAdvanced (searchForTheTruthAdvanced) where

import Arkham.Capability
import Arkham.Card
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.I18n
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
        chooseOrRunOneM iid $ cardI18n $ scope "searchForTheTruthAdvanced" do
          when canDrawCards do
            labeled' "draw" do
              drawCardsIfCan iid attrs 1
              doStep (x - 1) msg'
          labeled' "place" do
            push $ InvestigatorPlaceCluesOnLocation iid (toSource attrs) 1
            chooseOneM iid do
              for_ discardPile \card ->
                targeting card $ addToHand iid [toCard card]
            doStep (x - 1) msg'
          labeled' "skip" nothing

      pure e
    _ -> SearchForTheTruthAdvanced <$> liftRunMessage msg attrs
