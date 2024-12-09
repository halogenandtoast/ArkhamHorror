module Arkham.Enemy.Cards.HuntingDeepOne (huntingDeepOne, HuntingDeepOne (..)) where

import Arkham.Ability
import Arkham.Asset.Types (Field (..))
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Placement
import Arkham.Placement
import Arkham.Projection

newtype HuntingDeepOne = HuntingDeepOne EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

huntingDeepOne :: EnemyCard HuntingDeepOne
huntingDeepOne = enemy HuntingDeepOne Cards.huntingDeepOne (3, Static 3, 3) (1, 1)

instance HasModifiersFor HuntingDeepOne where
  getModifiersFor (HuntingDeepOne a) =
    modifySelect a (investigatorEngagedWith a) [CannotEnterVehicle AnyAsset, CannotGainResources]

instance HasAbilities HuntingDeepOne where
  getAbilities (HuntingDeepOne a) = extend a [mkAbility a 1 $ forced $ EnemyEngaged #after You (be a)]

instance RunMessage HuntingDeepOne where
  runMessage msg e@(HuntingDeepOne attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      placement <- field InvestigatorPlacement iid
      case placement of
        InVehicle aid -> do
          mlocation <- field AssetLocation aid
          for_ mlocation \location -> place iid (AtLocation location)
        _ -> push $ LoseResources iid (attrs.ability 1) 2
      pure e
    _ -> HuntingDeepOne <$> liftRunMessage msg attrs
