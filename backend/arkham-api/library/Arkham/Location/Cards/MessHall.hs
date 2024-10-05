module Arkham.Location.Cards.MessHall (messHall, MessHall (..)) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Message.Discard.Lifted
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype MessHall = MessHall LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

messHall :: LocationCard MessHall
messHall = location MessHall Cards.messHall 2 (PerPlayer 2)

instance HasAbilities MessHall where
  getAbilities (MessHall attrs) =
    extendRevealed1 attrs
      $ restrictedAbility attrs 1 Here
      $ forced
      $ SkillTestResult #after You (WhileInvestigating YourLocation) #success

instance RunMessage MessHall where
  runMessage msg l@(MessHall attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      chooseAndDiscardCard iid (attrs.ability 1)
      pure l
    _ -> MessHall <$> liftRunMessage msg attrs
