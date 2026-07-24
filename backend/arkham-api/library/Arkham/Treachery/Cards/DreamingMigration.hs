module Arkham.Treachery.Cards.DreamingMigration (dreamingMigration) where

import Arkham.Deck qualified as Deck
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.Scenario (findTopOfDiscard)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Movement
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype DreamingMigration = DreamingMigration TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dreamingMigration :: TreacheryCard DreamingMigration
dreamingMigration = treachery DreamingMigration Cards.dreamingMigration

instance RunMessage DreamingMigration where
  runMessage msg t@(DreamingMigration attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      enemies <- selectAny AnyEnemy
      if enemies
        then do
          sid <- getRandom
          revelationSkillTest sid iid attrs #willpower (Fixed 3)
        else gainSurge attrs
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      nearest <- select $ NearestEnemyToFallback iid AnyEnemy
      topEnemy <- findTopOfDiscard #enemy
      chooseOneM iid do
        unless (null nearest) do
          labeled "Move the nearest enemy to your location and it attacks you" do
            chooseOrRunOneM iid $ targets nearest \enemy -> do
              withLocationOf iid \lid -> enemyMoveToEdit attrs enemy lid \m -> m {moveMeans = OneAtATime}
              forTarget enemy msg
        for_ topEnemy \card ->
          labeled "Draw the topmost enemy in the encounter discard pile"
            $ drawCardFrom iid Deck.EncounterDiscard card
      pure t
    ForTarget (EnemyTarget enemy) (FailedThisSkillTest iid (isSource attrs -> True)) -> do
      initiateEnemyAttack enemy attrs iid
      pure t
    _ -> DreamingMigration <$> liftRunMessage msg attrs
