module Arkham.Act.Cards.TheUndergroundMaze (theUndergroundMaze) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Asset.Cards qualified as Assets
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Grid
import Arkham.Location.Types (Field (..))
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Scenarios.WrittenInRock.Helpers

newtype TheUndergroundMaze = TheUndergroundMaze ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theUndergroundMaze :: ActCard TheUndergroundMaze
theUndergroundMaze = act (2, A) TheUndergroundMaze Cards.theUndergroundMaze Nothing

instance HasAbilities TheUndergroundMaze where
  getAbilities (TheUndergroundMaze a) =
    scenarioI18n
      $ extend
        a
        [ mkAbility a 1 (FastAbility (GroupClueCost (PerPlayer 1) Anywhere))
            & restrict (DuringTurn You)
            & restrict (oneOf [HasAdjacentLocations LocationCanBeSwapped, exists LocationCanBeSlid])
        , groupLimit PerRound
            $ withI18nTooltip "theUndergroundMaze.slide"
            $ restricted a 2 (exists LocationCanBeSlid)
            $ actionAbilityWithCost (GroupResourceCost (PerPlayer 1) Anywhere)
        , withI18nTooltip "theUndergroundMaze.prismaticShard"
            $ mkAbility a 3 actionAbility
            & restrict
              ( exists
                  $ assetIs Assets.prismaticShardAlienMeteorite
                  <> AssetAt YourLocation
                  <> not_ (AssetControlledBy You)
              )
        , mkAbility a 4
            $ Objective
            $ forced
            $ VehicleEnters #after (assetIs Assets.mineCartReliableButBroken) (locationIs Locations.railExit)
        ]

instance RunMessage TheUndergroundMaze where
  runMessage msg a@(TheUndergroundMaze attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      locations <- select $ oneOf [LocationCanBeSwapped, LocationCanBeSlid]
      chooseTargetM iid locations $ handleTarget iid (attrs.ability 1)
      pure a
    HandleTargetChoice iid (isAbilitySource attrs 1 -> True) (LocationTarget lid) -> do
      pos <- fieldJust LocationPosition lid
      swappable <- select $ mapOneOf LocationInPosition (adjacentPositions pos) <> LocationCanBeSwapped
      slideLocations <-
        matches lid LocationCanBeSlid >>= \case
          False -> pure []
          True -> getEmptyPositions lid
      chooseOneM iid do
        targets swappable (swapLocations lid)
        for_ slideLocations \newPos ->
          gridLabeled (gridLabel newPos) $ push $ PlaceGrid (GridLocation newPos lid)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      locations <- select LocationCanBeSlid
      chooseTargetM iid locations $ handleTarget iid (attrs.ability 2)
      pure a
    HandleTargetChoice iid (isAbilitySource attrs 2 -> True) (LocationTarget lid) -> do
      slideLocations <-
        matches lid LocationCanBeSlid >>= \case
          False -> pure []
          True -> getEmptyPositions lid
      chooseOneM iid do
        for_ slideLocations \newPos ->
          gridLabeled (gridLabel newPos) $ push $ PlaceGrid (GridLocation newPos lid)
      pure a
    UseThisAbility iid (isSource attrs -> True) 3 -> do
      takeControlOfAsset iid =<< selectJust (assetIs Assets.prismaticShardAlienMeteorite)
      pure a
    UseThisAbility _iid (isSource attrs -> True) 4 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      simeon <- selectAny $ assetIs Assets.simeonAtwoodDedicatedTroublemaker <> AssetControlledBy Anyone
      leah <- selectAny $ assetIs Assets.leahAtwoodTheValeCook <> AssetControlledBy Anyone

      simeonAside <- selectAny $ SetAsideCardMatch $ cardIs Assets.simeonAtwoodDedicatedTroublemaker
      simeonInPlay <-
        selectAny $ assetIs Assets.simeonAtwoodDedicatedTroublemaker <> not_ (AssetControlledBy Anyone)

      leahAside <- selectAny $ SetAsideCardMatch $ cardIs Assets.leahAtwoodTheValeCook
      leahInPlay <- selectAny $ assetIs Assets.leahAtwoodTheValeCook <> not_ (AssetControlledBy Anyone)

      push
        $ if
          | simeon -> R1
          | leah -> R2
          | simeonAside || simeonInPlay -> R3
          | leahAside || leahInPlay -> R4
          | otherwise -> R5
      pure a
    _ -> TheUndergroundMaze <$> liftRunMessage msg attrs
