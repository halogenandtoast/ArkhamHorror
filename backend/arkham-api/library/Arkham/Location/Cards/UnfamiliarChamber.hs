module Arkham.Location.Cards.UnfamiliarChamber (unfamiliarChamber, UnfamiliarChamber (..)) where

import Arkham.Ability
import Arkham.Key
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenario.Types

newtype UnfamiliarChamber = UnfamiliarChamber LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unfamiliarChamber :: LocationCard UnfamiliarChamber
unfamiliarChamber = locationWith UnfamiliarChamber Cards.unfamiliarChamber 3 (PerPlayer 1) connectsToAdjacent

instance HasAbilities UnfamiliarChamber where
  getAbilities (UnfamiliarChamber attrs) =
    extendRevealed attrs [mkAbility attrs 1 $ forced $ RevealLocation #after Anyone (be attrs)]

instance RunMessage UnfamiliarChamber where
  runMessage msg l@(UnfamiliarChamber attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      let
        unrevealed = \case
          UnrevealedKey _ -> True
          _ -> False
      unrevealedKeys <- filter unrevealed . setToList <$> scenarioField ScenarioSetAsideKeys
      for_ (nonEmpty unrevealedKeys) $ sample >=> placeKey (toTarget attrs)
      pure l
    _ -> UnfamiliarChamber <$> liftRunMessage msg attrs
