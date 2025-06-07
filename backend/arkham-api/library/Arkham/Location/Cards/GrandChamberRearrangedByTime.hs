module Arkham.Location.Cards.GrandChamberRearrangedByTime (grandChamberRearrangedByTime) where

import Arkham.Ability
import Arkham.Campaigns.TheForgottenAge.Supply
import Arkham.Card
import Arkham.Direction
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Modifier

newtype GrandChamberRearrangedByTime = GrandChamberRearrangedByTime LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

grandChamberRearrangedByTime :: LocationCard GrandChamberRearrangedByTime
grandChamberRearrangedByTime =
  location GrandChamberRearrangedByTime Cards.grandChamberRearrangedByTime 2 (PerPlayer 1)
    & setLabel "grandChamber"
    & setConnectsTo (setFromList [LeftOf, RightOf])

instance HasAbilities GrandChamberRearrangedByTime where
  getAbilities (GrandChamberRearrangedByTime a) =
    extendRevealed1 a
      $ restricted a 1 (Here <> HasSupply Map <> thisExists a LocationWithoutClues) actionAbility

instance RunMessage GrandChamberRearrangedByTime where
  runMessage msg l@(GrandChamberRearrangedByTime attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      gameModifiers (attrs.ability 1) (toCard attrs) [ScenarioModifier "noVengeance", GainVictory 1]
      pure l
    _ -> GrandChamberRearrangedByTime <$> liftRunMessage msg attrs
