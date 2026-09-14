module Arkham.Asset.Assets.NauticalCharts (nauticalCharts) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Card
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.I18n
import Arkham.Investigator.Projection ()
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier

newtype NauticalCharts = NauticalCharts AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

nauticalCharts :: AssetCard NauticalCharts
nauticalCharts = asset NauticalCharts Cards.nauticalCharts

instance HasAbilities NauticalCharts where
  getAbilities (NauticalCharts x) =
    [ investigateAbility x 1 (DiscardCardCost (toCard x)) InYourHand
    , investigateAbility x 1 (exhaust x) (ControlsThis <> not_ InYourHand)
    ]

instance RunMessage NauticalCharts where
  runMessage msg a@(NauticalCharts attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      skillTestModifier sid (attrs.ability 1) iid (SkillModifier #intellect 1)
      investigate sid iid (attrs.ability 1)
      pure a
    PassedThisSkillTest iid (isSource attrs -> True) -> do
      let discardable = InHandOf NotForPlay (be iid) <> basic DiscardableCard
      skillTestCardOptionEdit attrs (preOriginalOption . optionWhenExists discardable) do
        doStep 1 msg
      pure a
    DoStep 1 (PassedThisSkillTest iid (isSource attrs -> True)) -> do
      hand <- select $ InHandOf NotForPlay (be iid) <> basic DiscardableCard
      unless (null hand) do
        withSkillTest \sid -> do
          chooseOneM iid do
            cardI18n $ questionLabeled "nauticalCharts.discard1CardFrom"
            labeledI "doNotDiscardCard" nothing
            targets hand \card -> do
              discardCard iid (attrs.ability 1) card
              skillTestModifier sid (attrs.ability 1) iid (DiscoveredClues 1)
      pure a
    _ -> NauticalCharts <$> liftRunMessage msg attrs
