module Arkham.Event.Events.Psychoanalysis3 (psychoanalysis3) where

import Arkham.Deck qualified as Deck
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Matcher
import Arkham.Strategy

newtype Psychoanalysis3 = Psychoanalysis3 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

psychoanalysis3 :: EventCard Psychoanalysis3
psychoanalysis3 = event Psychoanalysis3 Cards.psychoanalysis3

instance RunMessage Psychoanalysis3 where
  runMessage msg e@(Psychoanalysis3 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      iids <- select $ colocatedWith iid
      for_ iids \iid' -> do
        lookAt iid attrs iid' [fromTopOfDeck 3] #any (defer attrs IsNotDraw)
        healHorror iid' attrs 2
      pure e
    SearchFound _iid (isTarget attrs -> True) (Deck.InvestigatorDeck iid') cards -> do
      focusCards cards $ chooseOrRunOneM iid' $ targets cards $ drawCard iid'
      pure e
    _ -> Psychoanalysis3 <$> liftRunMessage msg attrs
