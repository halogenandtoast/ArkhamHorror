module Arkham.Homebrew.AgesUnwound.Acts.TheBoundary_052 (theBoundary_052) where

import Arkham.Ability
import Arkham.Act.Import.Lifted
import Arkham.Helpers.Investigator (getJustLocation)
import Arkham.Homebrew.AgesUnwound.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.AgesUnwound.CardDefs.Assets qualified as Assets
import Arkham.Homebrew.AgesUnwound.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.AgesUnwound.CardDefs.Treacheries qualified as Treacheries
import Arkham.Homebrew.AgesUnwound.Helpers (recordTheTimeFor)
import Arkham.Homebrew.AgesUnwound.Key
import Arkham.Homebrew.AgesUnwound.Traits (pattern Interior)
import Arkham.Matcher
import Arkham.Message.Lifted.Log (record)
import Arkham.Placement

newtype TheBoundary_052 = TheBoundary_052 ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theBoundary_052 :: ActCard TheBoundary_052
theBoundary_052 = act (1, A) TheBoundary_052 Cards.theBoundary_052 Nothing

viaFrontHallway :: ActAttrs -> Bool
viaFrontHallway a = toResultDefault False a.meta

instance HasAbilities TheBoundary_052 where
  getAbilities (TheBoundary_052 a) =
    [mkAbility a 1 $ Objective $ forced $ Enters #after You (LocationWithTrait Interior)]

instance RunMessage TheBoundary_052 where
  runMessage msg a@(TheBoundary_052 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      lid <- getJustLocation iid
      frontDoor <- lid <=~> locationIs Locations.frontHallway
      advancedWithOther attrs
      pure $ TheBoundary_052 $ attrs & setMeta frontDoor
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      record TheBoundaryIsBroken
      recordTheTimeFor TheBoundaryIsBroken

      if viaFrontHallway attrs
        then do
          record TheInvestigatorsUsedTheSchoolsFrontDoor
          recordTheTimeFor TheInvestigatorsUsedTheSchoolsFrontDoor

          selectEach (locationIs Locations.rearCorridors) removeLocationWithoutVictory
          placeSetAsideLocations_ [Locations.cafeteria, Locations.principalsOffice]

          frontHallway <- selectJust $ locationIs Locations.frontHallway
          createSetAsideEnemy_ Enemies.houndOfUnmaking frontHallway

          shuffleSetAsideIntoEncounterDeck (cardIs Treacheries.stirringTitan)

          advanceToAct attrs Cards.bigAndUgly_053 A
        else do
          record TheInvestigatorsSnuckIntoTheBackOfTheSchool

          selectEach (locationIs Locations.frontHallway) removeLocationWithoutVictory
          placeSetAsideLocations_
            [Locations.classroom_227, Locations.classroom_228, Locations.classroom_229]

          rearCorridors <- selectJust $ locationIs Locations.rearCorridors
          warding <- getSetAsideCard Assets.unstableWarding
          createAssetAt_ warding (AttachedToLocation rearCorridors)

          advanceToAct attrs Cards.doorwayToTheUnknown_054 A
      pure a
    _ -> TheBoundary_052 <$> liftRunMessage msg attrs
