module Arkham.Location.Cards.DimensionalDoorway (dimensionalDoorway) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Cost (getSpendableResources)
import Arkham.Helpers.Scenario (scenarioField)
import Arkham.I18n
import Arkham.Location.Cards qualified as Cards (dimensionalDoorway)
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenario.Types (Field (..))
import Arkham.Scenarios.LostInTimeAndSpace.Helpers
import Arkham.Trait

newtype DimensionalDoorway = DimensionalDoorway LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dimensionalDoorway :: LocationCard DimensionalDoorway
dimensionalDoorway = location DimensionalDoorway Cards.dimensionalDoorway 2 (PerPlayer 1)

instance HasAbilities DimensionalDoorway where
  getAbilities (DimensionalDoorway a) = extendRevealed1 a $ restricted a 1 Here $ forced $ TurnEnds #when You

instance RunMessage DimensionalDoorway where
  runMessage msg l@(DimensionalDoorway attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      encounterDiscard <- scenarioField ScenarioDiscard
      for_ (find (member Hex . toTraits) encounterDiscard) (drawCard iid)
      DimensionalDoorway <$> liftRunMessage msg attrs
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      resourceCount <- getSpendableResources iid
      chooseOrRunOneM iid do
        when (resourceCount >= 2) do
          withI18n $ countVar 2 $ labeled' "spendResources" $ spendResources iid 2
        scenarioI18n $ labeled' "dimensionalDoorway.shuffle" $ shuffleBackIntoEncounterDeck attrs
      pure l
    _ -> DimensionalDoorway <$> liftRunMessage msg attrs
