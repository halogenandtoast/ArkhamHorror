module Arkham.Event.Events.Psychoanalysis (psychoanalysis) where

import Arkham.Deck qualified as Deck
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Matcher
import Arkham.Strategy
import Arkham.Zone

newtype Psychoanalysis = Psychoanalysis EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

psychoanalysis :: EventCard Psychoanalysis
psychoanalysis = event Psychoanalysis Cards.psychoanalysis

instance RunMessage Psychoanalysis where
  runMessage msg e@(Psychoanalysis attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      iids <- select $ affectsColocated iid
      chooseTargetM iid iids \iid' ->
        lookAt iid attrs iid' [fromTopOfDeck 3] #any (defer attrs IsNotDraw)
      pure e
    SearchFound _iid (isTarget attrs -> True) (Deck.InvestigatorDeck iid') cards -> do
      focusCards cards do
        chooseOneM iid' do
          labeled "Draw 1 revealed card and shuffle the rest into your deck" do
            chooseOrRunOneM iid' $ targets cards $ drawCard iid'
          labeled "Heal 2 horror and return revealed cards to top in any order" do
            healHorror iid' attrs 2
            push $ UpdateSearchReturnStrategy iid' FromDeck PutBackInAnyOrder
      pure e
    _ -> Psychoanalysis <$> liftRunMessage msg attrs
