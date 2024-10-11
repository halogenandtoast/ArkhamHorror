module Arkham.Asset.Assets.KnightOfSwords3 (knightOfSwords3, KnightOfSwords3 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier

newtype KnightOfSwords3 = KnightOfSwords3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

knightOfSwords3 :: AssetCard KnightOfSwords3
knightOfSwords3 = asset KnightOfSwords3 Cards.knightOfSwords3

instance HasAbilities KnightOfSwords3 where
  getAbilities (KnightOfSwords3 a) =
    [ reactionAbility a 1 Free (WouldHaveSkillTestResult #when You AnySkillTest #success) ControlsThis
    , restrictedAbility a 2 InYourHand $ freeReaction (GameBegins #when)
    ]

instance RunMessage KnightOfSwords3 where
  runMessage msg a@(KnightOfSwords3 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withSkillTest \sid -> do
        chooseOneM iid do
          labeled "Discard Knight of Swords to get +3 instead" do
            skillTestModifier sid attrs iid (AnySkillValue 3)
            push RecalculateSkillTestResults
          labeled "Do not discard" do
            skillTestModifier sid attrs iid (AnySkillValue 1)
            push RecalculateSkillTestResults
      pure a
    InHand _ (UseThisAbility iid (isSource attrs -> True) 2) -> do
      putCardIntoPlay iid attrs
      pure a
    _ -> KnightOfSwords3 <$> liftRunMessage msg attrs
