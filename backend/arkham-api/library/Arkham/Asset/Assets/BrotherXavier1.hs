module Arkham.Asset.Assets.BrotherXavier1 (brotherXavier1, BrotherXavier1 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner hiding (AssetDefeated)
import Arkham.DamageEffect
import Arkham.Matcher hiding (NonAttackDamageEffect)
import Arkham.Prelude

newtype BrotherXavier1 = BrotherXavier1 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

brotherXavier1 :: AssetCard BrotherXavier1
brotherXavier1 = ally BrotherXavier1 Cards.brotherXavier1 (3, 3)

instance HasModifiersFor BrotherXavier1 where
  getModifiersFor (BrotherXavier1 a) = case a.controller of
    Just iid -> do
      controller <- modified_ a iid [SkillModifier #willpower 1]
      others <-
        modifySelect
          a
          (not_ (InvestigatorWithId iid) <> at_ (locationWithAsset a))
          [CanAssignDamageToAsset a.id, CanAssignHorrorToAsset a.id]
      pure $ controller <> others
    Nothing -> pure mempty

instance HasAbilities BrotherXavier1 where
  getAbilities (BrotherXavier1 a) =
    [controlledAbility a 1 CanDealDamage $ freeReaction (AssetDefeated #when ByAny $ be a)]

instance RunMessage BrotherXavier1 where
  runMessage msg a@(BrotherXavier1 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      enemies <- select $ EnemyAt $ locationWithInvestigator iid
      player <- getPlayer iid
      push
        $ chooseOrRunOne
          player
          [targetLabel eid [EnemyDamage eid $ nonAttack (attrs.ability 1) 2] | eid <- enemies]
      pure a
    _ -> BrotherXavier1 <$> runMessage msg attrs
