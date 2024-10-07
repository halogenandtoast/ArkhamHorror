module Arkham.Treachery.Cards.SlitheringBehindYou (SlitheringBehindYou (..), slitheringBehindYou) where

import Arkham.Card
import Arkham.Deck qualified as Deck
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection
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
        Nothing ->
          push
            $ FindEncounterCard
              iid
              (toTarget attrs)
              [FromEncounterDeck, FromEncounterDiscard, FromVoid]
              (cardIs Enemies.huntingHorror)
      pure t
    FoundEncounterCard iid target ec | isTarget attrs target -> do
      mlid <- field InvestigatorLocation iid
      for_ mlid \lid -> push (SpawnEnemyAtEngagedWith (EncounterCard ec) lid iid)
      pure t
    FoundEnemyInOutOfPlay VoidZone iid target eid | isTarget attrs target -> do
      push $ EnemySpawnEngagedWith eid $ InvestigatorWithId iid
      pure t
    _ -> SlitheringBehindYou <$> liftRunMessage msg attrs
