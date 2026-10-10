module Arkham.Homebrew.AgesUnwound.Acts.TheBoundary_160 (theBoundary_160) where

import Arkham.Ability
import Arkham.Act.Import.Lifted
import Arkham.ChaosToken (ChaosTokenFace (ElderThing))
import Arkham.Helpers.Investigator (getJustLocation)
import Arkham.Helpers.Modifiers (ModifierType (AdditionalCostToEnterMatching), modifySelect)
import Arkham.Homebrew.AgesUnwound.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.AgesUnwound.CardDefs.Assets qualified as Assets
import Arkham.Homebrew.AgesUnwound.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.AgesUnwound.CardDefs.Treacheries qualified as Treacheries
import Arkham.Homebrew.AgesUnwound.Key
import Arkham.Homebrew.AgesUnwound.Scenarios.AWorldTornDownAgain.Helpers (act1c)
import Arkham.Homebrew.AgesUnwound.Traits (pattern Interior)
import Arkham.Matcher
import Arkham.Placement

newtype TheBoundary_160 = TheBoundary_160 ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | Act 2a, and the scenario's /starting/ act on every branch but /Featureless
Streets/ (setup removes act 1a from the game there).
-}
theBoundary_160 :: ActCard TheBoundary_160
theBoundary_160 = act (2, A) TheBoundary_160 Cards.theBoundary_160 Nothing

{- | True once an investigator entered the Front Hallway rather than the Rear
Corridors, which is what the back branches on.
-}
viaFrontHallway :: ActAttrs -> Bool
viaFrontHallway a = toResultDefault False a.meta

{- | "If act 1c is in play, investigators must spend an additional
1[per_investigator] clues to enter an unrevealed [[Interior]] location."

Your past self is still working the boundary from the other side; once act 1c
advances (they break it) the surcharge lifts.
-}
instance HasModifiersFor TheBoundary_160 where
  getModifiersFor (TheBoundary_160 a) = whenM (selectAny act1c) do
    modifySelect
      a
      Anyone
      [ AdditionalCostToEnterMatching
          (UnrevealedLocation <> LocationWithTrait Interior)
          (GroupClueCost (PerPlayer 1) Anywhere)
      ]

-- | "Objective - If an investigator enters an [[Interior]] location, advance."
instance HasAbilities TheBoundary_160 where
  getAbilities (TheBoundary_160 a) =
    [mkAbility a 1 $ Objective $ forced $ Enters #after Anyone (LocationWithTrait Interior)]

instance RunMessage TheBoundary_160 where
  runMessage msg a@(TheBoundary_160 attrs) = runQueueT $ case msg of
    {- The back branches on /which/ Interior location was entered, so it is
    recorded here while the mover is still standing on it. Exactly one of the
    Front Hallway and the Rear Corridors is in play in this scenario -- setup (or
    act 1a) places the entrance the investigators did /not/ use a year ago. -}
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      lid <- getJustLocation iid
      frontDoor <- lid <=~> locationIs Locations.frontHallway
      advancedWithOther attrs
      pure $ TheBoundary_160 $ attrs & setMeta frontDoor
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      {- "If /the Myriad recruited a cruel sorcerer,/ spawn the set-aside Estravius
      Malone at the revealed [[Interior]] location."

      Resolved before the branches below put more Interior locations into play, so
      "the revealed Interior location" is unambiguously the one just entered. -}
      whenM (getHasRecord TheMyriadRecruitedACruelSorcerer) do
        selectOne (RevealedLocation <> LocationWithTrait Interior)
          >>= traverse_ (createSetAsideEnemy_ Enemies.estraviusMalone)

      if viaFrontHallway attrs
        then do
          {- "If you advanced by entering the Front Hallway: Put the set aside
          Cafeteria and Principal's Office locations into play. Spawn the set-aside
          Hound of Unmaking at the Front Hallway. Shuffle each set-aside copy of
          Stirring Titan into the encounter deck. Advance to Act 3a - Big and
          Ugly."

          Unlike Scenario III's act 2b, nothing is removed from the game: the
          other entrance was never in play. -}
          placeSetAsideLocations_ [Locations.cafeteria, Locations.principalsOffice]

          frontHallway <- selectJust $ locationIs Locations.frontHallway
          createSetAsideEnemy_ Enemies.houndOfUnmaking frontHallway

          shuffleSetAsideIntoEncounterDeck (cardIs Treacheries.stirringTitan)

          advanceToAct attrs Cards.bigAndUgly_161 A
        else do
          {- "If you advanced by entering the Rear Corridors: Put the set aside
          Classroom locations into play. Attach the set aside Unstable Warding
          story asset to Rear Corridors. If there is at least one [elder_thing]
          token in the chaos bag, place 1 horror on Unstable Warding. Advance to
          Act 3a - Doorway to the Unknown." -}
          placeSetAsideLocations_
            [Locations.classroom_227, Locations.classroom_228, Locations.classroom_229]

          {- The warding reads its host off 'AttachedToLocation' (falling back to
          'AtLocation'); any other placement leaves its action unusable. -}
          rearCorridors <- selectJust $ locationIs Locations.rearCorridors
          warding <- getSetAsideCard Assets.unstableWarding
          wardingId <- createAssetAt warding (AttachedToLocation rearCorridors)

          {- The warding has 2 sanity, so this one horror puts it one failed test
          away from flipping to /Backfire/. Scenario III does not do this; it is
          Scenario VI's own instruction. -}
          whenM (selectAny $ ChaosTokenFaceIs ElderThing)
            $ placeTokens attrs wardingId #horror 1

          advanceToAct attrs Cards.doorwayToTheUnknown_162 A
      pure a
    _ -> TheBoundary_160 <$> liftRunMessage msg attrs
