module Arkham.Asset.Cards.FireAxe
  ( FireAxe(..)
  , fireAxe
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Investigator.Types ( Field (..) )
import Arkham.Matcher
import Arkham.Projection
import Arkham.SkillType
import Arkham.Source

newtype FireAxe = FireAxe AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fireAxe :: AssetCard FireAxe
fireAxe = asset FireAxe Cards.fireAxe

instance HasModifiersFor FireAxe where
  getModifiersFor (InvestigatorTarget iid) (FireAxe a) | controlledBy a iid = do
    mSkillTestSource <- getSkillTestSource
    case mSkillTestSource of
      Just (SkillTestSource _ _ source (Just Action.Fight))
        | isSource a source -> do
          resourceCount <- field InvestigatorResources iid
          pure $ toModifiers a [ DamageDealt 1 | resourceCount == 0 ]
      _ -> pure []
  getModifiersFor _ _ = pure []

instance HasAbilities FireAxe where
  getAbilities (FireAxe a) =
    [ restrictedAbility a 1 ControlsThis
      $ ActionAbility (Just Action.Fight) (ActionCost 1)
    , restrictedAbility
        a
        2
        (ControlsThis
        <> DuringSkillTest (WhileAttackingAnEnemy AnyEnemy <> UsingThis)
        )
        (FastAbility (ResourceCost 1))
      & abilityLimitL
      .~ PlayerLimit PerTestOrAbility 3
    ]

instance RunMessage FireAxe where
  runMessage msg a@(FireAxe attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source ->
      a <$ push (ChooseFightEnemy iid source Nothing SkillCombat mempty False)
    UseCardAbility iid source 2 _ _ | isSource attrs source -> a <$ push
      (skillTestModifier
        attrs
        (InvestigatorTarget iid)
        (SkillModifier SkillCombat 2)
      )
    _ -> FireAxe <$> runMessage msg attrs
