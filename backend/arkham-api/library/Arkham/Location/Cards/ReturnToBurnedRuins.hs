module Arkham.Location.Cards.ReturnToBurnedRuins (returnToBurnedRuins) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype ReturnToBurnedRuins = ReturnToBurnedRuins LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

returnToBurnedRuins :: LocationCard ReturnToBurnedRuins
returnToBurnedRuins = locationWith ReturnToBurnedRuins Cards.returnToBurnedRuins 2 (Static 3) (labelL .~ "burnedRuins")

instance HasAbilities ReturnToBurnedRuins where
  getAbilities (ReturnToBurnedRuins a) =
    withDrawCardUnderneathAction a
      <> extendRevealed1
        a
        (mkAbility a 1 $ forced $ SkillTestResult #after You (WhileInvestigating (be a)) #success)

instance RunMessage ReturnToBurnedRuins where
  runMessage msg l@(ReturnToBurnedRuins attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignDamage iid (attrs.ability 1) 1
      pure l
    _ -> ReturnToBurnedRuins <$> liftRunMessage msg attrs
