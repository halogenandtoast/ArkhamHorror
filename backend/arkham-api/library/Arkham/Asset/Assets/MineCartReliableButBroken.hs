module Arkham.Asset.Assets.MineCartReliableButBroken (mineCartReliableButBroken) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Direction
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.Modifiers (modifySelf)
import Arkham.Helpers.Query (getLead)
import Arkham.Helpers.Vehicle (moveVehicle)
import Arkham.Location.Grid
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.Projection
import Arkham.Scenarios.WrittenInRock.Helpers

newtype MineCartReliableButBroken = MineCartReliableButBroken AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mineCartReliableButBroken :: AssetCard MineCartReliableButBroken
mineCartReliableButBroken = asset MineCartReliableButBroken Cards.mineCartReliableButBroken

instance HasAbilities MineCartReliableButBroken where
  getAbilities (MineCartReliableButBroken a) =
    [mkAbility a 1 $ forced $ PhaseEnds #when #enemy]

instance HasModifiersFor MineCartReliableButBroken where
  getModifiersFor (MineCartReliableButBroken a) = do
    let dir = toResultDefault East a.meta
    modifySelf a $ pure . UIModifier . Rotated $ case dir of
      North -> 270
      South -> 90
      East -> 0
      West -> 180

instance RunMessage MineCartReliableButBroken where
  runMessage msg a@(MineCartReliableButBroken attrs) = runQueueT $ case msg of
    ScenarioSpecific "rotate" val -> do
      let dir = toResultDefault East val
      pure $ MineCartReliableButBroken $ attrs & setMeta dir
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      let dir = toResultDefault East attrs.meta
      withLocationOf attrs \loc -> do
        pos <- fieldJust LocationPosition loc
        let newPos = updatePosition pos dir
        let
          movedOffTheRailLine =
            eachInvestigator \iid -> do
              sufferPhysicalTrauma iid 1
              kill (attrs.ability 1) iid
        selectOne (LocationInPosition newPos) >>= \case
          Nothing -> movedOffTheRailLine
          Just newLoc -> do
            rails <- getRails newLoc
            if oppositeDirection dir `elem` rails
              then do
                moveVehicle attrs loc newLoc
                scenarioSpecific_ "mineCartMoved"
              else movedOffTheRailLine
      pure a
    ScenarioSpecific "mineCartMoved" _ -> do
      withLocationOf attrs \loc -> do
        rails <- getRails loc
        let dir = toResultDefault East attrs.meta
        let
          turns = filter (`elem` rails)
            $ case dir of
              North -> [North, West, East]
              South -> [South, East, West]
              East -> [East, North, South]
              West -> [West, South, North]

        lead <- getLead
        chooseOrRunOneM lead $ scenarioI18n do
          questionLabeled' "mineCart.facing"
          when (North `elem` turns)
            $ labeled' "mineCart.faceNorth" do
              when (dir /= North) $ scenarioSpecific "rotate" North
          when (East `elem` turns)
            $ labeled' "mineCart.faceEast" do
              when (dir /= East) $ scenarioSpecific "rotate" East
          when (South `elem` turns)
            $ labeled' "mineCart.faceSouth" do
              when (dir /= South) $ scenarioSpecific "rotate" South
          when (West `elem` turns)
            $ labeled' "mineCart.faceWest" do
              when (dir /= West) $ scenarioSpecific "rotate" West
      pure a
    _ -> MineCartReliableButBroken <$> liftRunMessage msg attrs
