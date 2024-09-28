module Arkham.Enemy.Cards.EmergingDeepOne (emergingDeepOne, EmergingDeepOne (..)) where

import Arkham.Ability
import Arkham.Campaigns.TheInnsmouthConspiracy.Helpers
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Location (getLocationOf)
import Arkham.Location.FloodLevel
import Arkham.Matcher
import Arkham.Placement

newtype EmergingDeepOne = EmergingDeepOne EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

emergingDeepOne :: EnemyCard EmergingDeepOne
emergingDeepOne = enemy EmergingDeepOne Cards.emergingDeepOne (3, Static 2, 1) (1, 1)

instance HasAbilities EmergingDeepOne where
  getAbilities (EmergingDeepOne a) =
    extend1 a $ mkAbility a 1 $ forced $ EnemyEngaged #after (You <> at_ FullyFloodedLocation) (be a)

instance RunMessage EmergingDeepOne where
  runMessage msg e@(EmergingDeepOne attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> do
      getLocationOf attrs.id >>= \case
        Nothing -> error "We do not have a location yet..."
        Just loc ->
          getFloodLevel loc <&> \case
            FullyFlooded -> e
            _ -> EmergingDeepOne $ attrs & exhaustedL .~ True & placementL .~ AtLocation loc
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      initiateEnemyAttack attrs (attrs.ability 1) iid
      pure e
    _ -> EmergingDeepOne <$> liftRunMessage msg attrs
