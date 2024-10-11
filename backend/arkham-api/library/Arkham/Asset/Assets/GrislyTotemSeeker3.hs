module Arkham.Asset.Assets.GrislyTotemSeeker3 (
  grislyTotemSeeker3,
  grislyTotemSeeker3Effect,
  GrislyTotemSeeker3 (..),
) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Effect.Import
import Arkham.Helpers.Card
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Helpers.Window (getCommittedCard)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.SkillType

newtype GrislyTotemSeeker3 = GrislyTotemSeeker3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

grislyTotemSeeker3 :: AssetCard GrislyTotemSeeker3
grislyTotemSeeker3 = asset GrislyTotemSeeker3 Cards.grislyTotemSeeker3

instance HasAbilities GrislyTotemSeeker3 where
  getAbilities (GrislyTotemSeeker3 a) =
    [ restricted a 1 ControlsThis
        $ ReactionAbility (CommittedCard #after You AnyCard) (exhaust a)
    ]

toSkillLabel :: SkillIcon -> Text
toSkillLabel WildMinusIcon = "Choose Minus {wild}"
toSkillLabel WildIcon = "Choose {wild}"
toSkillLabel (SkillIcon sType) = case sType of
  SkillWillpower -> "Choose {willpower}"
  SkillIntellect -> "Choose {intellect}"
  SkillCombat -> "Choose {combat}"
  SkillAgility -> "Choose {agility}"

instance RunMessage GrislyTotemSeeker3 where
  runMessage msg a@(GrislyTotemSeeker3 attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (getCommittedCard -> card) _ -> do
      withSkillTest \sid -> do
        icons <- setFromList @(Set SkillIcon) <$> iconsForCard card
        chooseOrRunOneM iid do
          for_ (setToList icons) \icon -> do
            labeled (toSkillLabel icon)
              $ skillTestModifier sid (attrs.ability 1) card (AddSkillIcons [icon])
        createCardEffect Cards.grislyTotemSeeker3 Nothing (attrs.ability 1) sid
      pure a
    _ -> GrislyTotemSeeker3 <$> liftRunMessage msg attrs

newtype GrislyTotemSeeker3Effect = GrislyTotemSeeker3Effect EffectAttrs
  deriving anyclass (HasAbilities, HasModifiersFor, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

grislyTotemSeeker3Effect :: EffectArgs -> GrislyTotemSeeker3Effect
grislyTotemSeeker3Effect = cardEffect GrislyTotemSeeker3Effect Cards.grislyTotemSeeker3

instance RunMessage GrislyTotemSeeker3Effect where
  runMessage msg e@(GrislyTotemSeeker3Effect attrs) = runQueueT $ case msg of
    PassedSkillTest iid _ _ _ _ _ -> do
      withSkillTest \sid -> do
        when (isTarget sid attrs.target) do
          disable attrs
          drawCardsIfCan iid attrs.source 1
      pure e
    SkillTestEnds sid _ _ | isTarget sid attrs.target -> disableReturn e
    _ -> GrislyTotemSeeker3Effect <$> liftRunMessage msg attrs
