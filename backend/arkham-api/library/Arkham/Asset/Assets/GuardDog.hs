module Arkham.Asset.Assets.GuardDog (guardDog) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Window
import Arkham.Matcher hiding (NonAttackDamageEffect)

newtype GuardDog = GuardDog AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

guardDog :: AssetCard GuardDog
guardDog = ally GuardDog Cards.guardDog (3, 1)

instance HasAbilities GuardDog where
  getAbilities (GuardDog x) =
    [ controlled x 1 CanDealDamage
        $ freeReaction
        $ AssetDealtDamage #when (SourceIsEnemyAttack AnyEnemy) (be x)
    ]

instance RunMessage GuardDog where
  runMessage msg a@(GuardDog attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (getDamageSourceEnemy -> eid) _ -> do
      nonAttackEnemyDamage (Just iid) (attrs.ability 1) 1 eid
      pure a
    _ -> GuardDog <$> liftRunMessage msg attrs
