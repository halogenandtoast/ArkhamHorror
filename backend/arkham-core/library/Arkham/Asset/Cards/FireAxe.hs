module Arkham.Asset.Cards.FireAxe (FireAxe (..), fireAxe) where

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
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fireAxe :: AssetCard FireAxe
fireAxe = asset FireAxe Cards.fireAxe

instance HasModifiersFor FireAxe where
  getModifiersFor (InvestigatorTarget iid) (FireAxe a) | controlledBy a iid = do
    toModifiers a . toList <$> runMaybeT do
      guardM $ isAbilitySource a 1 <$> MaybeT getSkillTestSource
      Action.Fight <- MaybeT getSkillTestAction
      resourceCount <- lift $ field InvestigatorResources iid
      DamageDealt 1 <$ guard (resourceCount == 0)
  getModifiersFor _ _ = pure []

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
      pushM $ mkChooseFight iid (attrs.ability 1)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      push $ skillTestModifier attrs iid (SkillModifier #combat 2)
      pure a
    _ -> FireAxe <$> runMessage msg attrs
