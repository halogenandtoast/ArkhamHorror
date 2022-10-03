module Arkham.Asset.Cards.KeenEye
  ( keenEye
  , KeenEye(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Cost
import Arkham.Criteria
import Arkham.Effect.Window
import Arkham.EffectMetadata
import Arkham.SkillType
import Arkham.Target

newtype KeenEye = KeenEye AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

keenEye :: AssetCard KeenEye
keenEye = asset KeenEye Cards.keenEye

instance HasAbilities KeenEye where
  getAbilities (KeenEye a) =
    [ withTooltip
        "{fast} Spend 2 resources: You get +1 {intellect} until the end of the phase"
      $ restrictedAbility a 1 ControlsThis (FastAbility $ ResourceCost 2)
    , withTooltip
        "{fast} Spend 2 resources: You get +1 {combat} until the end of the phase"
      $ restrictedAbility a 2 ControlsThis (FastAbility $ ResourceCost 2)
    ]

instance RunMessage KeenEye where
  runMessage msg a@(KeenEye attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> a <$ push
      (CreateWindowModifierEffect
        EffectPhaseWindow
        (EffectModifiers $ toModifiers attrs [SkillModifier SkillIntellect 1])
        source
        (InvestigatorTarget iid)
      )
    UseCardAbility iid source 2 _ _ | isSource attrs source -> a <$ push
      (CreateWindowModifierEffect
        EffectPhaseWindow
        (EffectModifiers $ toModifiers attrs [SkillModifier SkillCombat 1])
        source
        (InvestigatorTarget iid)
      )
    _ -> KeenEye <$> runMessage msg attrs
