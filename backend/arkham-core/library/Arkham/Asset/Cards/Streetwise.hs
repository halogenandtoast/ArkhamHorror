module Arkham.Asset.Cards.Streetwise (
  streetwise,
  Streetwise (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.SkillType

newtype Streetwise = Streetwise AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

streetwise :: AssetCard Streetwise
streetwise = asset Streetwise Cards.streetwise

instance HasAbilities Streetwise where
  getAbilities (Streetwise a) =
    [ withTooltip
        "{fast} Spend 2 resources: You get +2 {intellect} for this skill test."
        $ restrictedAbility
          a
          1
          (ControlsThis <> DuringSkillTest AnySkillTest)
          (FastAbility $ ResourceCost 2)
    , withTooltip
        "{fast} Spend 2 resources: You get +2 {agility} for this skill test."
        $ restrictedAbility
          a
          2
          (ControlsThis <> DuringSkillTest AnySkillTest)
          (FastAbility $ ResourceCost 2)
    ]

instance RunMessage Streetwise where
  runMessage msg a@(Streetwise attrs) = case msg of
    UseCardAbility iid source 1 _ _
      | isSource attrs source ->
          a
            <$ push
              ( skillTestModifier
                  source
                  (InvestigatorTarget iid)
                  (SkillModifier SkillIntellect 2)
              )
    UseCardAbility iid source 2 _ _
      | isSource attrs source ->
          a
            <$ push
              ( skillTestModifier
                  source
                  (InvestigatorTarget iid)
                  (SkillModifier SkillAgility 2)
              )
    _ -> Streetwise <$> runMessage msg attrs
