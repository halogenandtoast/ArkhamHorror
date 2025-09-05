module Arkham.Event.Events.ParallelFates2 (parallelFates2) where

import Arkham.Capability
import Arkham.Card
import Arkham.Deck qualified as Deck
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Matcher
import Arkham.Strategy

newtype Metadata = Metadata {drawnCards :: [EncounterCard]}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype ParallelFates2 = ParallelFates2 (EventAttrs `With` Metadata)
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

parallelFates2 :: EventCard ParallelFates2
parallelFates2 = event (ParallelFates2 . (`with` Metadata [])) Cards.parallelFates2

instance RunMessage ParallelFates2 where
  runMessage msg e@(ParallelFates2 (attrs `With` meta)) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      xs <- selectTargets $ investigator_ can.manipulate.deck
      chooseTargetM iid (EncounterDeckTarget : xs) \target -> do
        lookAt iid attrs target [fromTopOfDeck 6] #any (defer attrs IsNotDraw)
      pure e
    SearchFound iid (isTarget attrs -> True) Deck.EncounterDeck cards -> do
      focusCards cards do
        chooseOneM iid do
          labeled "Shuffle them in" $ shuffleCardsIntoDeck Deck.EncounterDeck cards
          labeled "Put back in any order" do
            chooseOneAtATimeM iid do
              targets (onlyEncounterCards cards) $ putCardOnTopOfDeck iid Deck.EncounterDeck
      pure e
    SearchFound iid (isTarget attrs -> True) deck@(Deck.InvestigatorDeck _) cards -> do
      focusCards cards do
        chooseOneM iid do
          labeled "Shuffle them in" $ shuffleCardsIntoDeck deck cards
          labeled "Put back in any order" do
            chooseOneAtATimeM iid do
              targets (onlyPlayerCards cards) $ putCardOnTopOfDeck iid deck

      chooseOrRunOneM iid do
        labeled "Do not draw" nothing
        whenM (can.draw.cards iid) $ labeled "Draw 1 card" $ drawCards iid attrs 1
      pure e
    _ -> ParallelFates2 . (`with` meta) <$> liftRunMessage msg attrs
