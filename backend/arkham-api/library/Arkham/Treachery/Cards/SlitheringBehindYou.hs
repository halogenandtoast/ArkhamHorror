module Arkham.Treachery.Cards.SlitheringBehindYou (slitheringBehindYou) where

import Arkham.Card
import Arkham.Deck qualified as Deck
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Location
import Arkham.Matcher
import Arkham.Scenarios.TheMiskatonicMuseum.Helpers
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted
import Arkham.Zone

newtype SlitheringBehindYou = SlitheringBehindYou TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

slitheringBehindYou :: TreacheryCard SlitheringBehindYou
slitheringBehindYou = treachery SlitheringBehindYou Cards.slitheringBehindYou

instance RunMessage SlitheringBehindYou where
  runMessage msg t@(SlitheringBehindYou attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      getInPlayHuntingHorror >>= \case
        Just eid -> do
          placeDoom attrs eid 1
          shuffleDeck Deck.EncounterDeck
        Nothing -> findEncounterCardIn iid attrs (cardIs Enemies.huntingHorror) [#deck, #discard, #void]
      pure t
    FoundEncounterCard iid (isTarget attrs -> True) ec -> do
      withLocationOf iid \lid -> do
        push $ SpawnEnemyAtEngagedWith (EncounterCard ec) lid iid
      pure t
    FoundEnemyInOutOfPlay VoidZone iid (isTarget attrs -> True) eid -> do
      push $ EnemySpawnEngagedWith eid $ InvestigatorWithId iid
      pure t
    _ -> SlitheringBehindYou <$> liftRunMessage msg attrs
