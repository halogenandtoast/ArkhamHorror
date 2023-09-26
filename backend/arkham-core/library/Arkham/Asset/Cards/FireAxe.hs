module Arkham.Asset.Cards.FireAxe (
  FireAxe (..),
  fireAxe,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection
import Arkham.SkillType

newtype FireAxe = FireAxe AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fireAxe :: AssetCard FireAxe
fireAxe = asset FireAxe Cards.fireAxe

instance HasModifiersFor FireAxe where
  getModifiersFor (InvestigatorTarget iid) (FireAxe a) | controlledBy a iid = do
    mSource <- getSkillTestSource
    mAction <- getSkillTestAction
    case (mAction, mSource) of
      (Just Action.Fight, Just source) | isSource a source -> do
        resourceCount <- field InvestigatorResources iid
        pure $ toModifiers a [DamageDealt 1 | resourceCount == 0]
      _ -> pure []
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
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      push $ ChooseFightEnemy iid source Nothing SkillCombat mempty False
      pure a
    UseCardAbility iid source 2 _ _ | isSource attrs source -> do
      push $ skillTestModifier attrs iid (SkillModifier SkillCombat 2)
      pure a
    _ -> FireAxe <$> runMessage msg attrs
