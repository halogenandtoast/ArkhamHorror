module Arkham.Event.Events.RighteousHunt1 (righteousHunt1) where

import Arkham.Criteria
import Arkham.Enemy.Types (Field (..))
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.ChaosBag
import Arkham.Matcher
import Arkham.Message.Lifted.Move
import Arkham.Movement
import Arkham.Projection

newtype RighteousHunt1 = RighteousHunt1 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

righteousHunt1 :: EventCard RighteousHunt1
righteousHunt1 = event RighteousHunt1 Cards.righteousHunt1

instance RunMessage RighteousHunt1 where
  runMessage msg e@(RighteousHunt1 attrs) = runQueueT $ case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      enemies <-
        select
          $ CanEngageEnemyWithOverride
          $ overrideExists
          $ EnemyAt
          $ LocationWithAccessiblePath (toSource attrs) 2 (InvestigatorWithId iid) Anywhere

      chooseOneM iid do
        for_ enemies \enemy -> void $ runMaybeT do
          loc <- MaybeT $ field EnemyLocation enemy
          horror <- lift $ field EnemySanityDamage enemy
          n <- lift $ min horror <$> getRemainingBlessTokens
          lift do
            targeting enemy do
              moveToEdit attrs iid loc \m -> m {moveMeans = OneAtATime}
              handleTarget iid attrs enemy
              repeated n (addChaosToken #bless)

      pure e
    HandleTargetChoice iid (isSource attrs -> True) (EnemyTarget eid) -> do
      canEngage <- eid <=~> CanEngageEnemy (toSource attrs)
      when canEngage (enemyEngageInvestigator eid iid)
      pure e
    _ -> RighteousHunt1 <$> liftRunMessage msg attrs
