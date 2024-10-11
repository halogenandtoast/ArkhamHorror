module Arkham.Asset.Assets.GrislyTotemSurvivor3 (
  grislyTotemSurvivor3,
  grislyTotemSurvivor3Effect,
  GrislyTotemSurvivor3 (..),
) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Card
import Arkham.Effect.Import
import {-# SOURCE #-} Arkham.GameEnv (getCard)
import Arkham.Helpers.Card
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Helpers.Window (getCommittedCard)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.SkillType
import Arkham.Timing qualified as Timing

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

toSkillLabel :: SkillIcon -> Text
toSkillLabel WildMinusIcon = "Choose Minus {wild}"
toSkillLabel WildIcon = "Choose {wild}"
toSkillLabel (SkillIcon sType) = case sType of
  SkillWillpower -> "Choose {willpower}"
  SkillIntellect -> "Choose {intellect}"
  SkillCombat -> "Choose {combat}"
  SkillAgility -> "Choose {agility}"

instance RunMessage GrislyTotemSurvivor3 where
  runMessage msg a@(GrislyTotemSurvivor3 attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (getCommittedCard -> card) _ -> do
      withSkillTest \sid -> do
        icons <- setFromList @(Set SkillIcon) <$> iconsForCard card
        chooseOrRunOneM iid do
          for_ (setToList icons) \icon -> do
            labeled (toSkillLabel icon) do
              skillTestModifier sid attrs (toCardId card) (AddSkillIcons [icon])
              createCardEffect Cards.grislyTotemSurvivor3 (effectMetaTarget sid) attrs (CardIdTarget card.id)
      pure a
    _ -> GrislyTotemSurvivor3 <$> liftRunMessage msg attrs

newtype GrislyTotemSurvivor3Effect = GrislyTotemSurvivor3Effect EffectAttrs
  deriving anyclass (HasAbilities, HasModifiersFor, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

grislyTotemSurvivor3Effect :: EffectArgs -> GrislyTotemSurvivor3Effect
grislyTotemSurvivor3Effect = cardEffect GrislyTotemSurvivor3Effect Cards.grislyTotemSurvivor3

instance RunMessage GrislyTotemSurvivor3Effect where
  runMessage msg e@(GrislyTotemSurvivor3Effect attrs) = runQueueT $ case msg of
    FailedSkillTest _ _ _ SkillTestInitiatorTarget {} _ _ -> do
      withSkillTest \sid -> do
        when (attrs.metaTarget == Just (SkillTestTarget sid)) $ do
          case attrs.target of
            CardIdTarget cid -> do
              card <- getCard cid
              for_ (toCardOwner card) $ \iid ->
                push $ ReturnToHand iid (toTarget $ toCardId card)
            _ -> pure ()
      pure e
    SkillTestEnds sid _ _ | attrs.metaTarget == Just (SkillTestTarget sid) -> disableReturn e
    _ -> GrislyTotemSurvivor3Effect <$> liftRunMessage msg attrs
