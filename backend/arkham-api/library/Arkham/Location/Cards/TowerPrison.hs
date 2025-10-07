module Arkham.Location.Cards.TowerPrison (towerPrison) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), modified_)
import Arkham.Helpers.SkillTest (withSkillTest, withSkillTestInvestigator)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Placement

newtype TowerPrison = TowerPrison LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

towerPrison :: LocationCard TowerPrison
towerPrison = symbolLabel $ location TowerPrison Cards.towerPrison 4 (PerPlayer 2)

instance HasModifiersFor TowerPrison where
  getModifiersFor (TowerPrison a) = modified_ a ScenarioTarget [ForceConcealedPlacement (AtLocation a.id)]

-- The forced ability feels unnecessarily noisy, so unless there is an
-- interaction that wants it to be a forced ability we just skip it and make it
-- a modifier
instance HasAbilities TowerPrison where
  getAbilities (TowerPrison a) =
    extendRevealed
      a
      [ restricted a 2 (Here <> DuringSkillTest (SkillTestOfInvestigator $ investigatorAt a))
          $ FastAbility (ClueCost (Static 1))
      ]

instance RunMessage TowerPrison where
  runMessage msg l@(TowerPrison attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 2 -> do
      withSkillTest \sid -> do
        withSkillTestInvestigator \iid ->
          skillTestModifier sid (attrs.ability 2) iid (AnySkillValue 2)
      pure l
    _ -> TowerPrison <$> liftRunMessage msg attrs
