module Arkham.Homebrew.AgesUnwound.Acts.ADimensionOfItsOwn (aDimensionOfItsOwn) where

import Arkham.Act.Import.Lifted
import Arkham.Homebrew.AgesUnwound.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.AgesUnwound.Key
import Arkham.Homebrew.AgesUnwound.Scenarios.AWorldTornDownAgain.Helpers (presentDeck)
import Arkham.Matcher
import Arkham.Message.Lifted.Move (enemyMoveTo, forcedMoveTo)

newtype ADimensionOfItsOwn = ADimensionOfItsOwn ActAttrs
  deriving anyclass (IsAct, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | Act 1a. Only in play on the /the Myriad harnessed the power of another realm/
branch: otherwise setup removes this act and Featureless Streets from the game
and the scenario begins at act 2a.

The printed clue requirement is the act's own cost, which gives the engine's
spend-clues-to-advance ability.
-}
aDimensionOfItsOwn :: ActCard ADimensionOfItsOwn
aDimensionOfItsOwn =
  act (1, A) ADimensionOfItsOwn Cards.aDimensionOfItsOwn (Just $ GroupClueCost (PerPlayer 2) Anywhere)

instance RunMessage ADimensionOfItsOwn where
  runMessage msg a@(ADimensionOfItsOwn attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      {- "Put Front Gates, Sports Field, Children's Playground and Side Building
      into play. ... If /the investigators used the school's front door,/ put Rear
      Corridors into play. Otherwise, put Front Hallway into play."

      The front-door record picks the entrance your /past/ self is not using: in
      this scenario you must come in the other way (see act 2a's flavour, "You'll
      need to use a different entrance to your past self"). -}
      placeSetAsideLocations_
        [ Locations.frontGates_169
        , Locations.childrensPlayground_171
        , Locations.sideBuilding_170
        ]
      sportsField <- placeSetAsideLocation Locations.sportsField_172

      frontDoor <- getHasRecord TheInvestigatorsUsedTheSchoolsFrontDoor
      placeSetAsideLocation_
        $ if frontDoor then Locations.rearCorridors else Locations.frontHallway

      {- "Move each investigator and enemy in play to Sports Field."

      Scoped to investigators who are /at/ a location: a "returned to Arkham late"
      seat is 'Arkham.Placement.Unplaced' and not in play until the end of the
      first round, and the scenario's own end-of-round step is what brings them in
      (at the starting location, which this act has not changed). -}
      selectEach (InvestigatorAt Anywhere) \iid -> forcedMoveTo attrs iid sportsField
      -- A swarm card moves with its host, so only hosts are iterated.
      selectEach (InPlayEnemy $ not_ IsSwarm) \eid -> enemyMoveTo attrs eid sportsField

      -- "Remove Featureless Streets from the game." Not 'removeLocation': removed
      -- is not overcome, so a removal must never route to the victory display.
      selectEach (locationIs Locations.featurelessStreets) removeLocationWithoutVictory

      advanceActDeckN attrs presentDeck
      pure a
    _ -> ADimensionOfItsOwn <$> liftRunMessage msg attrs
