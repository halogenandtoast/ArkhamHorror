module Arkham.Location.Cards.FarAboveYourHouse (farAboveYourHouse) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Message.Discard.Lifted
import Arkham.Location.Cards qualified as Cards (farAboveYourHouse)
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype FarAboveYourHouse = FarAboveYourHouse LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

farAboveYourHouse :: LocationCard FarAboveYourHouse
farAboveYourHouse =
  location FarAboveYourHouse Cards.farAboveYourHouse 2 (PerPlayer 1)

instance HasAbilities FarAboveYourHouse where
  getAbilities (FarAboveYourHouse a) =
    extendRevealed1 a $ skillTestAbility $ mkAbility a 1 $ forced $ RevealLocation #after You (be a)

instance RunMessage FarAboveYourHouse where
  runMessage msg l@(FarAboveYourHouse attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #willpower (Fixed 4)
      pure l
    FailedThisSkillTestBy _ (isAbilitySource attrs 1 -> True) n -> do
      eachInvestigator \iid -> randomDiscardN iid (attrs.ability 1) n
      pure l
    _ -> FarAboveYourHouse <$> liftRunMessage msg attrs
