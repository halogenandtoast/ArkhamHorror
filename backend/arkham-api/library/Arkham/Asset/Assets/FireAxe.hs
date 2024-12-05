module Arkham.Asset.Assets.FireAxe (FireAxe (..), fireAxe) where

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Fight
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Projection

newtype FireAxe = FireAxe AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fireAxe :: AssetCard FireAxe
fireAxe = asset FireAxe Cards.fireAxe

instance HasModifiersFor FireAxe where
  getModifiersFor (FireAxe a) = case a.controller of
    Nothing -> pure mempty
    Just iid -> maybeModified_ a iid do
      guardM $ isAbilitySource a 1 <$> MaybeT getSkillTestSource
      Action.Fight <- MaybeT getSkillTestAction
      resourceCount <- lift $ field InvestigatorResources iid
      guard $ resourceCount == 0
      pure [DamageDealt 1]

instance HasAbilities FireAxe where
  getAbilities (FireAxe a) =
    [ fightAbility a 1 mempty ControlsThis
    , limitedAbility (PlayerLimit PerTestOrAbility 3)
        $ fastAbility a 2 (ResourceCost 1)
        $ ControlsThis
        <> DuringSkillTest (WhileAttackingAnEnemy AnyEnemy <> UsingThis)
    ]

instance RunMessage FireAxe where
  runMessage msg a@(FireAxe attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      pushM $ mkChooseFight sid iid (attrs.ability 1)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      withSkillTest \sid ->
        pushM $ skillTestModifier sid attrs iid (SkillModifier #combat 2)
      pure a
    _ -> FireAxe <$> runMessage msg attrs
