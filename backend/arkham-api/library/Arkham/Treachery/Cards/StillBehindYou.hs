module Arkham.Treachery.Cards.StillBehindYou (stillBehindYou) where

import Arkham.Card
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Matcher
import Arkham.Message.Lifted.Move
import Arkham.Movement
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted
import Arkham.Zone

newtype StillBehindYou = StillBehindYou TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stillBehindYou :: TreacheryCard StillBehindYou
stillBehindYou = treachery StillBehindYou Cards.stillBehindYou

instance RunMessage StillBehindYou where
  runMessage msg t@(StillBehindYou attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      copies <- selectCount $ VictoryDisplayCardMatch $ basic $ cardIs Cards.stillBehindYou
      sid <- getRandom
      revelationSkillTest sid iid attrs #agility (Fixed $ 2 + copies)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      selectOne (enemyIs Enemies.theInescapable) >>= \case
        Just enemy -> withLocationOf iid \lid -> do
          enemyMoveToEdit attrs enemy lid \m -> m {moveMeans = OneAtATime}
          forTarget enemy msg
        Nothing -> do
          addToVictory iid attrs
          findEncounterCardIn
            iid
            attrs
            (cardIs Enemies.theInescapable)
            ([FromEncounterDeck, FromEncounterDiscard] <> allOutOfPlayZones)
      pure t
    ForTarget (EnemyTarget enemy) (FailedThisSkillTest iid (isSource attrs -> True)) -> do
      enemyEngageInvestigator enemy iid
      pure t
    FoundEncounterCard iid (isTarget attrs -> True) card -> do
      withLocationOf iid \lid -> push $ SpawnEnemyAtEngagedWith (EncounterCard card) lid iid
      pure t
    FoundEnemyInOutOfPlay _ iid (isTarget attrs -> True) enemy -> do
      push $ EnemySpawnEngagedWith enemy $ InvestigatorWithId iid
      pure t
    _ -> StillBehindYou <$> liftRunMessage msg attrs
