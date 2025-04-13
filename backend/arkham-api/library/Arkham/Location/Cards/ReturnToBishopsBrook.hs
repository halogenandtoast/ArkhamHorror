module Arkham.Location.Cards.ReturnToBishopsBrook (returnToBishopsBrook) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype ReturnToBishopsBrook = ReturnToBishopsBrook LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

returnToBishopsBrook :: LocationCard ReturnToBishopsBrook
returnToBishopsBrook =
  locationWith ReturnToBishopsBrook Cards.returnToBishopsBrook 3 (Static 2) (labelL .~ "bishopsBrook")

instance HasAbilities ReturnToBishopsBrook where
  getAbilities (ReturnToBishopsBrook a) =
    withDrawCardUnderneathAction a
      <> extendRevealed1 a (mkAbility a 1 $ forced $ RevealLocation #after You (be a))

instance RunMessage ReturnToBishopsBrook where
  runMessage msg l@(ReturnToBishopsBrook attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #agility (Fixed 2)
      pure l
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      assignDamage iid (attrs.ability 1) 2
      pure l
    _ -> ReturnToBishopsBrook <$> liftRunMessage msg attrs
