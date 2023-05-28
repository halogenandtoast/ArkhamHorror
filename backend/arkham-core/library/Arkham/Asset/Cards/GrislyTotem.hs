module Arkham.Asset.Cards.GrislyTotem (
  grislyTotem,
  GrislyTotem (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.Matcher
import Arkham.SkillType
import Arkham.Timing qualified as Timing
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype GrislyTotem = GrislyTotem AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

grislyTotem :: AssetCard GrislyTotem
grislyTotem = asset GrislyTotem Cards.grislyTotem

instance HasAbilities GrislyTotem where
  getAbilities (GrislyTotem a) =
    [ restrictedAbility a 1 ControlsThis $
        ReactionAbility
          (CommittedCard Timing.After You AnyCard)
          (ExhaustCost $ toTarget a)
    ]

getWindowCard :: [Window] -> Card
getWindowCard [] = error "missing card"
getWindowCard (Window _ (Window.CommittedCard _ c) : _) = c
getWindowCard (_ : ws) = getWindowCard ws

toSkillLabel :: SkillIcon -> Text
toSkillLabel WildMinusIcon = "Choose Minus {wild}"
toSkillLabel WildIcon = "Choose {wild}"
toSkillLabel (SkillIcon sType) = case sType of
  SkillWillpower -> "Choose {willpower}"
  SkillIntellect -> "Choose {intellect}"
  SkillCombat -> "Choose {combat}"
  SkillAgility -> "Choose {agility}"

instance RunMessage GrislyTotem where
  runMessage msg a@(GrislyTotem attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (getWindowCard -> card) _ -> do
      icons <- setFromList @(Set SkillIcon) <$> iconsForCard card
      push $
        chooseOrRunOne
          iid
          [ Label
            (toSkillLabel icon)
            [skillTestModifier attrs (toCardId card) (AddSkillIcons [icon])]
          | icon <- setToList icons
          ]
      pure a
    _ -> GrislyTotem <$> runMessage msg attrs
