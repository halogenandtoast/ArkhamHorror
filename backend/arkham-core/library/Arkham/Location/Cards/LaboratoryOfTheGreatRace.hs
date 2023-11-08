module Arkham.Location.Cards.LaboratoryOfTheGreatRace (
  laboratoryOfTheGreatRace,
  LaboratoryOfTheGreatRace (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.ScenarioLogKey
import Arkham.SkillType

newtype LaboratoryOfTheGreatRace = LaboratoryOfTheGreatRace LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

laboratoryOfTheGreatRace :: LocationCard LaboratoryOfTheGreatRace
laboratoryOfTheGreatRace =
  location
    LaboratoryOfTheGreatRace
    Cards.laboratoryOfTheGreatRace
    2
    (PerPlayer 1)

instance HasAbilities LaboratoryOfTheGreatRace where
  getAbilities (LaboratoryOfTheGreatRace attrs) =
    withBaseAbilities
      attrs
      [ restrictedAbility attrs 1 (Here <> NoCluesOnThis)
          $ ActionAbility []
          $ ActionCost 1
      ]

instance RunMessage LaboratoryOfTheGreatRace where
  runMessage msg l@(LaboratoryOfTheGreatRace attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      push $ beginSkillTest iid (toAbilitySource attrs 1) iid SkillAgility 3
      pure l
    PassedSkillTest _ _ (isAbilitySource attrs 1 -> True) SkillTestInitiatorTarget {} _ _ ->
      do
        push $ Remember ActivatedTheDevice
        pure l
    FailedSkillTest _ _ (isAbilitySource attrs 1 -> True) SkillTestInitiatorTarget {} _ _ ->
      do
        push $ PlaceClues (toAbilitySource attrs 1) (toTarget attrs) 1
        pure l
    _ -> LaboratoryOfTheGreatRace <$> runMessage msg attrs
