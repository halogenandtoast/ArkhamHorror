module Arkham.Location.Cards.LaboratoryOfTheGreatRace (laboratoryOfTheGreatRace) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Message.Lifted.Log
import Arkham.ScenarioLogKey

newtype LaboratoryOfTheGreatRace = LaboratoryOfTheGreatRace LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

laboratoryOfTheGreatRace :: LocationCard LaboratoryOfTheGreatRace
laboratoryOfTheGreatRace = location LaboratoryOfTheGreatRace Cards.laboratoryOfTheGreatRace 2 (PerPlayer 1)

instance HasAbilities LaboratoryOfTheGreatRace where
  getAbilities (LaboratoryOfTheGreatRace a) =
    extendRevealed1 a $ skillTestAbility $ restricted a 1 (Here <> NoCluesOnThis) actionAbility

instance RunMessage LaboratoryOfTheGreatRace where
  runMessage msg l@(LaboratoryOfTheGreatRace attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #agility (Fixed 3)
      pure l
    PassedThisSkillTest _ (isAbilitySource attrs 1 -> True) -> do
      remember ActivatedTheDevice
      pure l
    FailedThisSkillTest _ (isAbilitySource attrs 1 -> True) -> do
      placeClues (attrs.ability 1) attrs 1
      pure l
    _ -> LaboratoryOfTheGreatRace <$> liftRunMessage msg attrs
