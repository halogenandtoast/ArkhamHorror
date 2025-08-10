module Arkham.Treachery.Cards.UnexpectedAmbush (unexpectedAmbush) where

import Arkham.Helpers.Location (withLocationOf)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Movement
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype UnexpectedAmbush = UnexpectedAmbush TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unexpectedAmbush :: TreacheryCard UnexpectedAmbush
unexpectedAmbush = treachery UnexpectedAmbush Cards.unexpectedAmbush

instance RunMessage UnexpectedAmbush where
  runMessage msg t@(UnexpectedAmbush attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      enemies <- select AnyInPlayEnemy
      if null enemies
        then assignDamageAndHorror iid attrs 1 1
        else do
          sid <- getRandom
          chooseOneM iid do
            for_ [#intellect, #agility] \kind -> do
              skillLabeled kind $ revelationSkillTest sid iid attrs kind (Fixed 4)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      nearestEnemies <- select $ NearestEnemyTo iid AnyEnemy
      withLocationOf iid \location -> do
        chooseOrRunOneM iid do
          targets nearestEnemies \enemy -> do
            enemyMoveToEdit attrs enemy location \m -> m {moveMeans = OneAtATime}
            forTarget enemy msg
      pure t
    ForTarget (EnemyTarget enemy) (FailedThisSkillTestBy iid (isSource attrs -> True) n) -> do
      whenMatch enemy (EnemyAt $ locationWithInvestigator iid) do
        enemyEngageInvestigator enemy iid
        when (n >= 3) $ initiateEnemyAttack enemy attrs iid
      pure t
    _ -> UnexpectedAmbush <$> liftRunMessage msg attrs
