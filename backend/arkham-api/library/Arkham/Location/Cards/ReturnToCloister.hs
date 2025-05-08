module Arkham.Location.Cards.ReturnToCloister (returnToCloister) where

import Arkham.Ability
import Arkham.Helpers.SkillTest.Lifted
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.ScenarioLogKey

newtype ReturnToCloister = ReturnToCloister LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

returnToCloister :: LocationCard ReturnToCloister
returnToCloister = location ReturnToCloister Cards.returnToCloister 1 (PerPlayer 1)

instance HasAbilities ReturnToCloister where
  getAbilities (ReturnToCloister a) =
    extendRevealed1 a $ skillTestAbility $ restricted a 1 (Here <> NoCluesOnThis) parleyAction_

instance RunMessage ReturnToCloister where
  runMessage msg l@(ReturnToCloister attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      chooseOneM iid do
        for_ [#willpower, #intellect] \kind ->
          skillLabeled kind $ parley sid iid (attrs.ability 1) iid kind (Fixed 4)
      pure l
    PassedThisSkillTest _ (isAbilitySource attrs 1 -> True) -> do
      remember FoundTheTowerKey
      pure l
    _ -> ReturnToCloister <$> liftRunMessage msg attrs
