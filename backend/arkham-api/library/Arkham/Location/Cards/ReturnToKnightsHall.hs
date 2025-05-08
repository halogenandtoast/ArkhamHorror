module Arkham.Location.Cards.ReturnToKnightsHall (returnToKnightsHall) where

import Arkham.Ability
import Arkham.Helpers.SkillTest.Lifted
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.ScenarioLogKey

newtype ReturnToKnightsHall = ReturnToKnightsHall LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

returnToKnightsHall :: LocationCard ReturnToKnightsHall
returnToKnightsHall = location ReturnToKnightsHall Cards.returnToKnightsHall 3 (PerPlayer 1)

instance HasAbilities ReturnToKnightsHall where
  getAbilities (ReturnToKnightsHall a) =
    extendRevealed1 a $ skillTestAbility $ restricted a 1 (Here <> NoCluesOnThis) parleyAction_

instance RunMessage ReturnToKnightsHall where
  runMessage msg l@(ReturnToKnightsHall attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      chooseOneM iid do
        for_ [#combat, #agility] \kind ->
          skillLabeled kind $ parley sid iid (attrs.ability 1) iid kind (Fixed 3)
      pure l
    PassedThisSkillTest _ (isAbilitySource attrs 1 -> True) -> do
      remember FoundAGuide
      pure l
    _ -> ReturnToKnightsHall <$> liftRunMessage msg attrs
