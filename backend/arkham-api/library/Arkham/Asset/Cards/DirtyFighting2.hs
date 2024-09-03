module Arkham.Asset.Cards.DirtyFighting2 (dirtyFighting2, DirtyFighting2 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (EnemyEvaded)
import Arkham.Helpers.Modifiers (ModifierType (..), maybeModified)
import Arkham.Helpers.SkillTest (getSkillTestAction, getSkillTestTarget)
import Arkham.Helpers.Window (evadedEnemy)
import Arkham.Matcher

newtype DirtyFighting2 = DirtyFighting2 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dirtyFighting2 :: AssetCard DirtyFighting2
dirtyFighting2 = asset DirtyFighting2 Cards.dirtyFighting2

instance HasModifiersFor DirtyFighting2 where
  getModifiersFor (InvestigatorTarget iid) (DirtyFighting2 attrs) =
    maybeModified attrs do
      guard $ attrs `controlledBy` iid
      EnemyTarget eid <- MaybeT getSkillTestTarget
      liftGuardM $ eid <=~> ExhaustedEnemy
      action <- MaybeT getSkillTestAction
      guard $ action `elem` [#fight, #evade, #parley]
      pure [AnySkillValue 2]
  getModifiersFor _ _ = pure []

instance HasAbilities DirtyFighting2 where
  getAbilities (DirtyFighting2 a) =
    [ restrictedAbility a 1 ControlsThis
        $ ReactionAbility (EnemyEvaded #after You $ ignoreAloofFightOverride AnyEnemy) (exhaust a)
    ]

instance RunMessage DirtyFighting2 where
  runMessage msg a@(DirtyFighting2 attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (evadedEnemy -> enemy) _ -> do
      nextSkillTestModifier (attrs.ability 1) enemy IgnoreAloof
      performActionAction iid (attrs.ability 1) #fight
      pure a
    _ -> DirtyFighting2 <$> liftRunMessage msg attrs
