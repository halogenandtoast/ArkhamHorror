module Arkham.Asset.Assets.DirtyFighting2 (dirtyFighting2) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (EnemyEvaded)
import Arkham.Helpers.Modifiers (ModifierType (..), maybeModified_)
import Arkham.Helpers.SkillTest (getSkillTestAction, getSkillTestTargetedEnemy, isParley)
import Arkham.Helpers.Window (evadedEnemy)
import Arkham.Matcher

newtype DirtyFighting2 = DirtyFighting2 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dirtyFighting2 :: AssetCard DirtyFighting2
dirtyFighting2 = asset DirtyFighting2 Cards.dirtyFighting2

instance HasModifiersFor DirtyFighting2 where
  getModifiersFor (DirtyFighting2 a) = for_ a.controller \iid -> do
    maybeModified_ a iid do
      eid <- MaybeT getSkillTestTargetedEnemy
      guardM $ eid <=~> ExhaustedEnemy
      maction <- lift getSkillTestAction
      guardM $ (maybe False (`elem` [#fight, #evade, #parley]) maction ||) <$> isParley
      pure [AnySkillValue 2]

instance HasAbilities DirtyFighting2 where
  getAbilities (DirtyFighting2 a) =
    [ restricted a 1 ControlsThis
        $ triggered (EnemyEvaded #after You $ ignoreAloofFightOverride AnyEnemy) (exhaust a)
    ]

instance RunMessage DirtyFighting2 where
  runMessage msg a@(DirtyFighting2 attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (evadedEnemy -> enemy) _ -> do
      nextSkillTestModifier iid (attrs.ability 1) enemy IgnoreAloof
      nextSkillTestModifier iid (attrs.ability 1) iid (MustFight enemy)
      doStep 1 msg
      pure a
    DoStep _ (UseThisAbility iid (isSource attrs -> True) 1) -> do
      performActionAction iid (attrs.ability 1) #fight
      pure a
    _ -> DirtyFighting2 <$> liftRunMessage msg attrs
