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

{- | True once an investigator entered the Front Hallway rather than the Rear
Corridors, which is what the back branches on.
-}
viaFrontHallway :: ActAttrs -> Bool
viaFrontHallway a = toResultDefault False a.meta

-- | "Objective - If an investigator enters an [[Interior]] location, advance."
instance HasAbilities TheBoundary_052 where
  getAbilities (TheBoundary_052 a) =
    [mkAbility a 1 $ Objective $ forced $ Enters #after Anyone (LocationWithTrait Interior)]

instance RunMessage TheBoundary_052 where
  runMessage msg a@(TheBoundary_052 attrs) = runQueueT $ case msg of
    {- The back branches on /which/ Interior location was entered, so it is
    recorded here while the mover is still standing on it. The window fires only
    on entry, and the Front Hallway and the Rear Corridors are the only Interior
    locations in play during act 1, so "where are they now" answers "where did
    they go". -}
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      lid <- getJustLocation iid
      frontDoor <- lid <=~> locationIs Locations.frontHallway
      advancedWithOther attrs
      pure $ TheBoundary_052 $ attrs & setMeta frontDoor
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      -- "In your Campaign Log, record that the boundary is broken. Next to this,
      -- record the time."
      record TheBoundaryIsBroken
      recordTheTimeFor TheBoundaryIsBroken

      if viaFrontHallway attrs
        then do
          {- "If you advanced by entering the Front Hallway: ... record that the
          investigators used the school's front door. Remove Rear Corridors from
          the game. Put the set-aside Cafeteria and Principal's Office locations
          into play. Spawn the set-aside Hound of Unmaking at the Front Hallway.
          Shuffle each set-aside copy of Stirring Titan into the encounter deck.
          Advance to Act 2a - Big and Ugly." -}
          record TheInvestigatorsUsedTheSchoolsFrontDoor
          recordTheTimeFor TheInvestigatorsUsedTheSchoolsFrontDoor

          {- "Removed from the game" is not "overcome", so a Victory X location
          must not score (ArkhamDB Rules Reference, Victory Display: a victory
          location scores only if it is in play, revealed and clueless at the end
          of the scenario). 'removeLocation' would divert it to the victory
          display; Rear Corridors has no victory icon, but Front Hallway below
          does. -}
          selectEach (locationIs Locations.rearCorridors) removeLocationWithoutVictory
          placeSetAsideLocations_ [Locations.cafeteria, Locations.principalsOffice]

          frontHallway <- selectJust $ locationIs Locations.frontHallway
          createSetAsideEnemy_ Enemies.houndOfUnmaking frontHallway

          shuffleSetAsideIntoEncounterDeck (cardIs Treacheries.stirringTitan)

          advanceToAct attrs Cards.bigAndUgly_053 A
        else do
          {- "If you advanced by entering the Rear Corridors: ... record that the
          investigators snuck into the back of the school. Remove Front Hallway
          from the game. Put each of the set-aside Classroom locations into play.
          Attach the set-aside Unstable Warding story asset to Rear Corridors.
          Advance to Act 2a - Doorway to the Unknown."

          No time is recorded beside this one; the act's own
          /the boundary is broken/ above carries it. -}
          record TheInvestigatorsSnuckIntoTheBackOfTheSchool

          selectEach (locationIs Locations.frontHallway) removeLocationWithoutVictory
          placeSetAsideLocations_
            [Locations.classroom_227, Locations.classroom_228, Locations.classroom_229]

          {- The warding reads its host off 'AttachedToLocation' (falling back to
          'AtLocation'); any other placement leaves its action unusable. -}
          rearCorridors <- selectJust $ locationIs Locations.rearCorridors
          warding <- getSetAsideCard Assets.unstableWarding
          createAssetAt_ warding (AttachedToLocation rearCorridors)

          advanceToAct attrs Cards.doorwayToTheUnknown_054 A
      pure a
    _ -> TheBoundary_052 <$> liftRunMessage msg attrs
