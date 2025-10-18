module Arkham.Asset.Assets.BrotherXavier1 (brotherXavier1) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (AssetDefeated)
import Arkham.Helpers.Modifiers
import Arkham.Matcher hiding (NonAttackDamageEffect)
import Arkham.Message.Lifted.Choose

newtype BrotherXavier1 = BrotherXavier1 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

brotherXavier1 :: AssetCard BrotherXavier1
brotherXavier1 = ally BrotherXavier1 Cards.brotherXavier1 (3, 3)

instance HasModifiersFor BrotherXavier1 where
  getModifiersFor (BrotherXavier1 a) = for_ a.controller \iid -> do
    modified_ a iid [SkillModifier #willpower 1]
    modifySelect
      a
      (not_ (InvestigatorWithId iid) <> at_ (locationWithAsset a))
      [CanAssignDamageToAsset a.id, CanAssignHorrorToAsset a.id]

instance HasAbilities BrotherXavier1 where
  getAbilities (BrotherXavier1 a) =
    [ controlled a 1 (canDamageEnemyAt (a.ability 1) YourLocation)
        $ freeReaction (AssetDefeated #when ByAny $ be a)
    ]

instance RunMessage BrotherXavier1 where
  runMessage msg a@(BrotherXavier1 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      chooseDamageEnemy iid (attrs.ability 1) (locationWithInvestigator iid) AnyEnemy 2
      pure a
    _ -> BrotherXavier1 <$> liftRunMessage msg attrs
