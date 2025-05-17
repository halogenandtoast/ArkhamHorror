module Arkham.Event.Events.ParallelFates (parallelFates) where

import Arkham.Card
import Arkham.ChaosToken
import Arkham.Deck qualified as Deck
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Strategy

newtype Metadata = Metadata {drawnCards :: [EncounterCard]}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype ParallelFates = ParallelFates (EventAttrs `With` Metadata)
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

parallelFates :: EventCard ParallelFates
parallelFates = event (ParallelFates . (`with` Metadata [])) Cards.parallelFates

instance RunMessage ParallelFates where
  runMessage msg e@(ParallelFates (attrs `With` meta)) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      lookAt iid attrs EncounterDeckTarget [fromTopOfDeck 4] #any (defer attrs IsNotDraw)
      pure e
    SearchFound _ (isTarget attrs -> True) _ cards | notNull cards -> do
      focusCards cards $ requestChaosTokens attrs.owner attrs 1
      pure $ ParallelFates (attrs `With` Metadata (onlyEncounterCards cards))
    RequestedChaosTokens (isSource attrs -> True) (Just iid) (map chaosTokenFace -> tokens) -> do
      if any (`elem` [Skull, Cultist, Tablet, ElderThing, AutoFail]) tokens
        then chooseOneM iid do
          labeled "Shuffle back in" $ shuffleCardsIntoDeck Deck.EncounterDeck (drawnCards meta)
        else chooseOneAtATimeM iid $ targets (drawnCards meta) $ putCardOnTopOfDeck iid Deck.EncounterDeck
      pure e
    _ -> ParallelFates . (`with` meta) <$> liftRunMessage msg attrs
