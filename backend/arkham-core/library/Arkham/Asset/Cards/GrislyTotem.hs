module Arkham.Asset.Cards.GrislyTotem
  ( grislyTotem
  , GrislyTotem(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.Cost
import Arkham.Criteria
import Arkham.Matcher
import Arkham.SkillType
import Arkham.Timing qualified as Timing
import Arkham.Window ( Window (..) )
import Arkham.Window qualified as Window

newtype GrislyTotem = GrislyTotem AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

grislyTotem :: AssetCard GrislyTotem
grislyTotem = asset GrislyTotem Cards.grislyTotem

instance HasAbilities GrislyTotem where
  getAbilities (GrislyTotem a) =
    [ restrictedAbility a 1 ControlsThis $ ReactionAbility
        (CommittedCard Timing.After You AnyCard)
        (ExhaustCost $ toTarget a)
    ]

getCard :: [Window] -> Card
getCard [] = error "missing card"
getCard (Window _ (Window.CommittedCard _ c) : _) = c
getCard (_ : ws) = getCard ws

toSkillLabel :: SkillIcon -> Text
toSkillLabel WildIcon = "{wild}"
toSkillLabel (SkillIcon sType) = case sType of
  SkillWillpower -> "{willpower}"
  SkillIntellect -> "{intellect}"
  SkillCombat -> "{combat}"
  SkillAgility -> "{agility}"

instance RunMessage GrislyTotem where
  runMessage msg a@(GrislyTotem attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (getCard -> card) _ -> do
      icons <- setFromList @(HashSet SkillIcon) <$> iconsForCard card
      push $ chooseOrRunOne
        iid
        [ Label
            (toSkillLabel icon)
            [skillTestModifier attrs (toCardId card) (AddSkillIcons [icon])]
        | icon <- setToList icons
        ]
      pure a
    _ -> GrislyTotem <$> runMessage msg attrs
