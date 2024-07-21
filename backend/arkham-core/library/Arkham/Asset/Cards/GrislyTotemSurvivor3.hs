module Arkham.Asset.Cards.GrislyTotemSurvivor3 (
  grislyTotemSurvivor3,
  grislyTotemSurvivor3Effect,
  GrislyTotemSurvivor3 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.Effect.Import
import Arkham.Helpers.Card
import Arkham.Matcher
import Arkham.SkillType
import Arkham.Timing qualified as Timing
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype GrislyTotemSurvivor3 = GrislyTotemSurvivor3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

grislyTotemSurvivor3 :: AssetCard GrislyTotemSurvivor3
grislyTotemSurvivor3 = asset GrislyTotemSurvivor3 Cards.grislyTotemSurvivor3

instance HasAbilities GrislyTotemSurvivor3 where
  getAbilities (GrislyTotemSurvivor3 a) =
    [ restrictedAbility a 1 ControlsThis
        $ ReactionAbility
          (CommittedCard Timing.After You AnyCard)
          (ExhaustCost $ toTarget a)
    ]

getCard :: [Window] -> Card
getCard [] = error "missing card"
getCard ((windowType -> Window.CommittedCard _ c) : _) = c
getCard (_ : ws) = getCard ws

toSkillLabel :: SkillIcon -> Text
toSkillLabel WildMinusIcon = "Choose Minus {wild}"
toSkillLabel WildIcon = "Choose {wild}"
toSkillLabel (SkillIcon sType) = case sType of
  SkillWillpower -> "Choose {willpower}"
  SkillIntellect -> "Choose {intellect}"
  SkillCombat -> "Choose {combat}"
  SkillAgility -> "Choose {agility}"

instance RunMessage GrislyTotemSurvivor3 where
  runMessage msg a@(GrislyTotemSurvivor3 attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (getCard -> card) _ -> do
      withSkillTest \sid -> do
        icons <- setFromList @(Set SkillIcon) <$> iconsForCard card
        player <- getPlayer iid
        push
          $ chooseOrRunOne
            player
            [ Label
              (toSkillLabel icon)
              [ skillTestModifier sid attrs (toCardId card) (AddSkillIcons [icon])
              , createCardEffect Cards.grislyTotemSurvivor3 (effectMetaTarget sid) attrs (CardTarget card)
              ]
            | icon <- setToList icons
            ]
      pure a
    _ -> GrislyTotemSurvivor3 <$> runMessage msg attrs

newtype GrislyTotemSurvivor3Effect = GrislyTotemSurvivor3Effect EffectAttrs
  deriving anyclass (HasAbilities, HasModifiersFor, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

grislyTotemSurvivor3Effect :: EffectArgs -> GrislyTotemSurvivor3Effect
grislyTotemSurvivor3Effect = cardEffect GrislyTotemSurvivor3Effect Cards.grislyTotemSurvivor3

instance RunMessage GrislyTotemSurvivor3Effect where
  runMessage msg e@(GrislyTotemSurvivor3Effect attrs@EffectAttrs {..}) =
    case msg of
      FailedSkillTest _ _ _ SkillTestInitiatorTarget {} _ _ -> do
        withSkillTest \sid -> do
          when (attrs.metaTarget == Just (SkillTestTarget sid)) $ do
            case effectTarget of
              CardTarget card -> for_ (toCardOwner card) $ \iid ->
                push $ ReturnToHand iid (toTarget $ toCardId card)
              _ -> pure ()
        pure e
      SkillTestEnds sid _ _ | attrs.metaTarget == Just (SkillTestTarget sid) -> do
        push $ DisableEffect effectId
        pure e
      _ -> GrislyTotemSurvivor3Effect <$> runMessage msg attrs
