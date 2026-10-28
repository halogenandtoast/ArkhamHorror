module Arkham.Homebrew.AgesUnwound.Acts.ADimensionOfItsOwn (aDimensionOfItsOwn) where

import Arkham.Act.Import.Lifted
import Arkham.Homebrew.AgesUnwound.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.AgesUnwound.Key
import Arkham.Homebrew.AgesUnwound.Scenarios.AWorldTornDownAgain.Helpers (presentDeck)
import Arkham.Matcher
import Arkham.Message.Lifted.Move (enemyMoveTo, forcedMoveTo)

newtype ADimensionOfItsOwn = ADimensionOfItsOwn ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

aDimensionOfItsOwn :: ActCard ADimensionOfItsOwn
aDimensionOfItsOwn = act (1, A) ADimensionOfItsOwn Cards.aDimensionOfItsOwn (groupClueCost $ PerPlayer 2)

instance RunMessage ADimensionOfItsOwn where
  runMessage msg a@(ADimensionOfItsOwn attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      placeSetAsideLocations_
        [ Locations.frontGates_169
        , Locations.childrensPlayground_171
        , Locations.sideBuilding_170
        ]
      sportsField <- placeSetAsideLocation Locations.sportsField_172

      frontDoor <- getHasRecord TheInvestigatorsUsedTheSchoolsFrontDoor
      placeSetAsideLocation_
        $ if frontDoor then Locations.rearCorridors else Locations.frontHallway

      selectEach (InvestigatorAt Anywhere) \iid -> forcedMoveTo attrs iid sportsField
      selectEach (InPlayEnemy $ not_ IsSwarm) \eid -> enemyMoveTo attrs eid sportsField
      selectEach (locationIs Locations.featurelessStreets) removeLocationWithoutVictory

      advanceActDeckN attrs presentDeck
      pure a
    _ -> ADimensionOfItsOwn <$> liftRunMessage msg attrs
