module Arkham.Location.Cards.ReturnToGrandGuignol (returnToGrandGuignol) where

import Arkham.Ability
import Arkham.I18n
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype ReturnToGrandGuignol = ReturnToGrandGuignol LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

returnToGrandGuignol :: LocationCard ReturnToGrandGuignol
returnToGrandGuignol = location ReturnToGrandGuignol Cards.returnToGrandGuignol 5 (PerPlayer 1)

instance HasAbilities ReturnToGrandGuignol where
  getAbilities (ReturnToGrandGuignol a) =
    extendRevealed1 a
      $ groupLimit PerGame
      $ restricted a 1 Here
      $ actionAbilityWithCost (ResourceCost 5)

instance RunMessage ReturnToGrandGuignol where
  runMessage msg l@(ReturnToGrandGuignol attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      mAgenda <- selectOne AgendaWithAnyDoom
      chooseOrRunOneM iid $ withI18n do
        countVar 1 $ labeled' "placeAgendaDoom" $ placeDoomOnAgenda 1
        for_ mAgenda \agenda -> do
          countVar 1 $ labeled' "removeAgendaDoom" $ removeDoom (attrs.ability 1) agenda 1
      pure l
    _ -> ReturnToGrandGuignol <$> liftRunMessage msg attrs
