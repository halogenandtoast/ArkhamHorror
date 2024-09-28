module Arkham.Enemy.Cards.EmergingDeepOne (emergingDeepOne, EmergingDeepOne (..)) where

import Arkham.Ability
import Arkham.Campaigns.TheInnsmouthConspiracy.Helpers
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Enemy.Types (delayEngagementL)
import Arkham.Helpers.Location (getLocationOf)
import Arkham.Location.FloodLevel
import Arkham.Matcher
import Arkham.Placement

newtype EmergingDeepOne = EmergingDeepOne EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

emergingDeepOne :: EnemyCard EmergingDeepOne
emergingDeepOne =
  enemyWith EmergingDeepOne Cards.emergingDeepOne (3, Static 2, 1) (1, 1) (delayEngagementL .~ True)

instance HasAbilities EmergingDeepOne where
  getAbilities (EmergingDeepOne a) =
    extend1 a $ mkAbility a 1 $ forced $ EnemyEngaged #after (You <> at_ FullyFloodedLocation) (be a)

instance RunMessage EmergingDeepOne where
  runMessage msg e@(EmergingDeepOne attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> do
      getLocationOf attrs.id >>= \case
        Nothing -> error "We do not have a location yet..."
        Just loc ->
          getFloodLevel loc >>= \case
            FullyFlooded -> do
              enemyCheckEngagement attrs.id
              pure $ EmergingDeepOne $ attrs & delayEngagementL .~ False
            _ ->
              pure
                $ EmergingDeepOne
                $ attrs
                & (exhaustedL .~ True)
                & (placementL .~ AtLocation loc)
                & (delayEngagementL .~ False)
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      initiateEnemyAttack attrs (attrs.ability 1) iid
      pure e
    _ -> EmergingDeepOne <$> liftRunMessage msg attrs
