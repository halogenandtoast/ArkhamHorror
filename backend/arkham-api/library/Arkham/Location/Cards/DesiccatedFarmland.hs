module Arkham.Location.Cards.DesiccatedFarmland (desiccatedFarmland) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype DesiccatedFarmland = DesiccatedFarmland LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

desiccatedFarmland :: LocationCard DesiccatedFarmland
desiccatedFarmland = locationWith DesiccatedFarmland Cards.desiccatedFarmland 3 (PerPlayer 1) connectsToAdjacent

instance HasModifiersFor DesiccatedFarmland where
  getModifiersFor (DesiccatedFarmland a) = modifySelect a (investigatorAt (toId a)) [CannotGainResources]

instance HasAbilities DesiccatedFarmland where
  getAbilities (DesiccatedFarmland a) =
    extendRevealed1 a
      $ restricted a 1 (Here <> youExist InvestigatorWithAnyResources)
      $ forced
      $ TurnEnds #after You

instance RunMessage DesiccatedFarmland where
  runMessage msg l@(DesiccatedFarmland attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      loseResources iid (attrs.ability 1) 1
      pure l
    _ -> DesiccatedFarmland <$> liftRunMessage msg attrs
