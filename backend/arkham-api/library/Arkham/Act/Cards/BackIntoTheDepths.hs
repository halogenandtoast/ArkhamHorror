module Arkham.Act.Cards.BackIntoTheDepths (BackIntoTheDepths (..), backIntoTheDepths) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.CampaignLogKey
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Query
import Arkham.Key
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Grid
import Arkham.Matcher

newtype BackIntoTheDepths = BackIntoTheDepths ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

backIntoTheDepths :: ActCard BackIntoTheDepths
backIntoTheDepths = act (1, A) BackIntoTheDepths Cards.backIntoTheDepths Nothing

instance HasAbilities BackIntoTheDepths where
  getAbilities (BackIntoTheDepths a) =
    extend
      a
      [ restricted
          a
          1
          ( EachUndefeatedInvestigator (at_ $ locationIs Locations.gatewayToYhanthlei)
              <> foldMap (exists . InvestigatorWithKey) [BlueKey, RedKey, YellowKey, GreenKey]
          )
          $ Objective
          $ FastAbility Free
      ]

instance RunMessage BackIntoTheDepths where
  runMessage msg a@(BackIntoTheDepths attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      selectEach (not_ $ locationIs Locations.gatewayToYhanthlei) removeLocation
      n <- getPlayerCount
      lairOfDagonCard <- getSetAsideCard Locations.lairOfDagonIntoTheMaelstrom
      lairOfHydraCard <- getSetAsideCard Locations.lairOfHydra
      sanctums <- shuffle =<< getSetAsideCardsMatching "Y'ha-nthlei Sanctum"
      yhanthlei <- shuffle =<< getSetAsideCardsMatching "Y'ha-nthlei"
      (lairOfDagon, lairOfHydra) <- case n of
        4 -> do
          let sanctumPositions = [Pos (-3) (-1), Pos 3 (-1), Pos (-2) (-2), Pos 2 (-2)]
          for_ (zip sanctumPositions sanctums) (uncurry placeLocationInGrid_)

          let yhanthleiPositions = [Pos (-2) (-1), Pos (-1) (-1), Pos 0 (-1), Pos 1 (-1), Pos 2 (-1), Pos 0 (-2), Pos 0 (-3)]
          for_ (zip yhanthleiPositions yhanthlei) (uncurry placeLocationInGrid_)
          (,)
            <$> placeLocationInGrid (Pos 1 (-3)) lairOfDagonCard
            <*> placeLocationInGrid (Pos (-1) (-3)) lairOfHydraCard
        3 -> do
          let sanctumPositions = [Pos (-3) (-1), Pos 3 (-1), Pos (-2) (-2), Pos 2 (-2)]
          for_ (zip sanctumPositions sanctums) (uncurry placeLocationInGrid_)

          let yhanthleiPositions = [Pos (-2) (-1), Pos (-1) (-1), Pos 0 (-1), Pos 1 (-1), Pos 2 (-1), Pos 0 (-2)]
          for_ (zip yhanthleiPositions yhanthlei) (uncurry placeLocationInGrid_)

          (,)
            <$> placeLocationInGrid (Pos 1 (-2)) lairOfDagonCard
            <*> placeLocationInGrid (Pos (-1) (-2)) lairOfHydraCard
        2 -> do
          let sanctumPositions = [Pos (-2) (-1), Pos 2 (-1), Pos (-1) (-2), Pos 1 (-2)]
          for_ (zip sanctumPositions sanctums) (uncurry placeLocationInGrid_)

          let yhanthleiPositions = [Pos (-1) (-1), Pos 0 (-1), Pos 1 (-1), Pos 0 (-2), Pos 0 (-3)]
          for_ (zip yhanthleiPositions yhanthlei) (uncurry placeLocationInGrid_)

          (,)
            <$> placeLocationInGrid (Pos 1 (-3)) lairOfDagonCard
            <*> placeLocationInGrid (Pos (-1) (-3)) lairOfHydraCard
        1 -> do
          let sanctumPositions = [Pos (-2) (-1), Pos 2 (-1), Pos (-2) (-2), Pos 2 (-2)]
          for_ (zip sanctumPositions sanctums) (uncurry placeLocationInGrid_)

          let yhanthleiPositions = [Pos (-1) (-1), Pos 0 (-1), Pos 1 (-1), Pos 0 (-2)]
          for_ (zip yhanthleiPositions yhanthlei) (uncurry placeLocationInGrid_)

          (,)
            <$> placeLocationInGrid (Pos 1 (-2)) lairOfDagonCard
            <*> placeLocationInGrid (Pos (-1) (-2)) lairOfHydraCard
        _ -> error "Wrong number of players"

      getSetAsideCard Enemies.hydraDeepInSlumber >>= (`createEnemyAt_` lairOfHydra)

      dagonHasAwakened <- getHasRecord DagonHasAwakened
      let dagon =
            if dagonHasAwakened
              then Enemies.dagonAwakenedAndEnragedIntoTheMaelstrom
              else Enemies.dagonDeepInSlumberIntoTheMaelstrom
      getSetAsideCard dagon >>= (`createEnemyAt_` lairOfDagon)

      advanceActDeck attrs
      pure a
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure a
    _ -> BackIntoTheDepths <$> liftRunMessage msg attrs
