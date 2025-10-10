module Arkham.Location.Cards.WindingGulf (windingGulf) where

import Arkham.Ability
import Arkham.Direction
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.BeforeTheBlackThrone.Cosmos.Types
import Arkham.Scenarios.BeforeTheBlackThrone.Helpers

newtype WindingGulf = WindingGulf LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

windingGulf :: LocationCard WindingGulf
windingGulf = locationWith WindingGulf Cards.windingGulf 2 (Static 2) (connectsToL .~ adjacentLocations)

instance HasAbilities WindingGulf where
  getAbilities (WindingGulf a) = extendRevealed a [cosmos a 1, scenarioI18n $ hauntedI "windingGulf.haunted" a 2]

instance RunMessage WindingGulf where
  runMessage msg l@(WindingGulf attrs) = runQueueT $ case msg of
    RunCosmos iid (is attrs -> True) msgs -> do
      locations <- select RevealedLocation
      positions <- mapMaybeM findLocationInCosmos locations
      let leftmost = mins $ map (toSnd (\(Pos x _) -> x)) positions
      allEmpty <- concatForM leftmost \pos ->
        getEmptyPositionsInDirections pos [GridUp, GridDown, GridLeft, GridRight]

      azathoth <- selectJust $ IncludeOmnipotent $ enemyIs Enemies.azathoth
      msgs' <- capture $ chooseOneM iid $ scenarioI18n do
        labeled' "windingGulf.move" $ pushAll msgs
        labeled' "windingGulf.doNotMove" $ placeDoom attrs azathoth 1

      chooseCosmos attrs iid allEmpty msgs'
      pure l
    Do (PlaceCosmos _ (is attrs -> True) cloc) -> do
      handleCosmos attrs cloc
      pure l
    UseThisAbility iid (is attrs -> True) 2 -> do
      assignDamageAndHorror iid attrs 1 1
      pure l
    _ -> WindingGulf <$> liftRunMessage msg attrs
