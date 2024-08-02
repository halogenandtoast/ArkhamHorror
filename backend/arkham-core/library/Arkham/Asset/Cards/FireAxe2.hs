module Arkham.Asset.Cards.FireAxe2 (FireAxe2 (..), fireAxe2) where

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Fight
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Projection

newtype FireAxe2 = FireAxe2 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fireAxe2 :: AssetCard FireAxe2
fireAxe2 = asset FireAxe2 Cards.fireAxe2

instance HasModifiersFor FireAxe2 where
  getModifiersFor (InvestigatorTarget iid) (FireAxe2 a) | controlledBy a iid = do
    toModifiers a . toList <$> runMaybeT do
      guardM $ isAbilitySource a 1 <$> MaybeT getSkillTestSource
      Action.Fight <- MaybeT getSkillTestAction
      resourceCount <- lift $ field InvestigatorResources iid
      DamageDealt 1 <$ guard (resourceCount == 0)
  getModifiersFor _ _ = pure []

instance HasAbilities FireAxe2 where
  getAbilities (FireAxe2 a) =
    [ fightAbility a 1 mempty ControlsThis
    , limitedAbility (PlayerLimit PerTestOrAbility 3)
        $ fastAbility a 2 (ResourceCost 1)
        $ ControlsThis
        <> DuringSkillTest (WhileAttackingAnEnemy AnyEnemy <> UsingThis)
    ]

instance RunMessage FireAxe2 where
  runMessage msg a@(FireAxe2 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      pushM $ mkChooseFight sid iid (attrs.ability 1)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      withSkillTest \sid ->
        push $ skillTestModifier sid attrs iid (SkillModifier #combat 2)
      pure a
    _ -> FireAxe2 <$> runMessage msg attrs
