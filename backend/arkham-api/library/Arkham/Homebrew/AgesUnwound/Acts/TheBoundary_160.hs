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

theBoundary_160 :: ActCard TheBoundary_160
theBoundary_160 = act (2, A) TheBoundary_160 Cards.theBoundary_160 Nothing

viaFrontHallway :: ActAttrs -> Bool
viaFrontHallway a = toResultDefault False a.meta

instance HasModifiersFor TheBoundary_160 where
  getModifiersFor (TheBoundary_160 a) = whenM (selectAny act1c) do
    modifySelect
      a
      Anyone
      [ AdditionalCostToEnterMatching
          (UnrevealedLocation <> LocationWithTrait Interior)
          (GroupClueCost (PerPlayer 1) Anywhere)
      ]

instance HasAbilities TheBoundary_160 where
  getAbilities (TheBoundary_160 a) =
    [mkAbility a 1 $ Objective $ forced $ Enters #after You (LocationWithTrait Interior)]

instance RunMessage TheBoundary_160 where
  runMessage msg a@(TheBoundary_160 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      lid <- getJustLocation iid
      frontDoor <- lid <=~> locationIs Locations.frontHallway
      advancedWithOther attrs
      pure $ TheBoundary_160 $ attrs & setMeta frontDoor
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      whenM (getHasRecord TheMyriadRecruitedACruelSorcerer) do
        selectOne (RevealedLocation <> LocationWithTrait Interior)
          >>= traverse_ (createSetAsideEnemy_ Enemies.estraviusMalone)

      if viaFrontHallway attrs
        then do
          placeSetAsideLocations_ [Locations.cafeteria, Locations.principalsOffice]

          frontHallway <- selectJust $ locationIs Locations.frontHallway
          createSetAsideEnemy_ Enemies.houndOfUnmaking frontHallway

          shuffleSetAsideIntoEncounterDeck (cardIs Treacheries.stirringTitan)

          advanceToAct attrs Cards.bigAndUgly_161 A
        else do
          placeSetAsideLocations_
            [Locations.classroom_227, Locations.classroom_228, Locations.classroom_229]

          rearCorridors <- selectJust $ locationIs Locations.rearCorridors
          warding <- getSetAsideCard Assets.unstableWarding
          wardingId <- createAssetAt warding (AttachedToLocation rearCorridors)

          whenM (selectAny $ ChaosTokenFaceIs ElderThing)
            $ placeTokens attrs wardingId #horror 1

          advanceToAct attrs Cards.doorwayToTheUnknown_162 A
      pure a
    _ -> TheBoundary_160 <$> liftRunMessage msg attrs
