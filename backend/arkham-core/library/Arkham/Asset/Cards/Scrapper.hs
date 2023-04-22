module Arkham.Asset.Cards.Scrapper
  ( scrapper
  , Scrapper(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Effect.Window
import Arkham.EffectMetadata
import Arkham.Matcher
import Arkham.SkillType

newtype Scrapper = Scrapper AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

scrapper :: AssetCard Scrapper
scrapper = asset Scrapper Cards.scrapper

instance HasAbilities Scrapper where
  getAbilities (Scrapper a) =
    [ withTooltip
        "{fast} Spend 1 resource: You get +1 {combat} for this skill test."
      $ restrictedAbility a 1 (ControlsThis <> DuringSkillTest AnySkillTest)
      $ FastAbility
      $ ResourceCost 1
    , withTooltip
        "{fast} Spend 1 resource: You get +1 {agility} for this skill test."
      $ restrictedAbility a 2 (ControlsThis <> DuringSkillTest AnySkillTest)
      $ FastAbility
      $ ResourceCost 1
    ]

instance RunMessage Scrapper where
  runMessage msg a@(Scrapper attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> a <$ push
      (CreateWindowModifierEffect
        EffectPhaseWindow
        (EffectModifiers $ toModifiers attrs [SkillModifier SkillCombat 1])
        source
        (InvestigatorTarget iid)
      )
    UseCardAbility iid source 2 _ _ | isSource attrs source -> a <$ push
      (CreateWindowModifierEffect
        EffectPhaseWindow
        (EffectModifiers $ toModifiers attrs [SkillModifier SkillAgility 1])
        source
        (InvestigatorTarget iid)
      )
    _ -> Scrapper <$> runMessage msg attrs
