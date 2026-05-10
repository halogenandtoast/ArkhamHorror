module Arkham.Event.Events.ParallelFates2 (parallelFates2) where

import Arkham.Capability
import Arkham.Deck qualified as Deck
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Matcher
import Arkham.Strategy
import Arkham.Zone

newtype ParallelFates2 = ParallelFates2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

parallelFates2 :: EventCard ParallelFates2
parallelFates2 = event ParallelFates2 Cards.parallelFates2

instance RunMessage ParallelFates2 where
  runMessage msg e@(ParallelFates2 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      xs <- selectTargets $ investigator_ can.manipulate.deck
      chooseTargetM iid (EncounterDeckTarget : xs) \target -> do
        lookAt iid attrs target [fromTopOfDeck 6] #any (defer attrs IsNotDraw)
      pure e
    SearchFound iid (isTarget attrs -> True) Deck.EncounterDeck cards -> do
      focusCards cards do
        chooseOneM iid do
          labeledI "shuffleThemIn" nothing
          labeledI "putBackInAnyOrder" do
            push $ UpdateSearchReturnStrategy iid FromDeck PutBackInAnyOrder
      pure e
    SearchFound iid (isTarget attrs -> True) (Deck.InvestigatorDeck iid') cards -> do
      focusCards cards do
        chooseOneM iid do
          labeledI "shuffleThemIn" nothing
          labeledI "putBackInAnyOrder" do
            push $ UpdateSearchReturnStrategy iid' FromDeck PutBackInAnyOrder
      afterSearch do
        chooseOrRunOneM iid' do
          labeledI "doNotDraw" nothing
          whenM (can.draw.cards iid') $ labeledI "drawCardOne" $ drawCards iid' attrs 1
      pure e
    _ -> ParallelFates2 <$> liftRunMessage msg attrs
