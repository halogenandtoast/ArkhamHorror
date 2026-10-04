module Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Acts.BackIntoTheDepthsV2 (backIntoTheDepthsV2) where

import Arkham.Ability
import Arkham.Act.Import.Lifted
import Arkham.Campaigns.TheInnsmouthConspiracy.Key
import Arkham.Enemy.CardDefs.TheInnsmouthConspiracy.IntoTheMaelstrom qualified as Enemies
import Arkham.Helpers.Query
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.CardDefs.Treacheries qualified as HBTreacheries
import Arkham.Key
import Arkham.Location.CardDefs.TheInnsmouthConspiracy.IntoTheMaelstrom qualified as Locations
import Arkham.Location.Grid
import Arkham.Matcher hiding (DuringTurn)

newtype BackIntoTheDepthsV2 = BackIntoTheDepthsV2 ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

backIntoTheDepthsV2 :: ActCard BackIntoTheDepthsV2
backIntoTheDepthsV2 = act (1, A) BackIntoTheDepthsV2 Cards.backIntoTheDepthsV2 Nothing

instance HasAbilities BackIntoTheDepthsV2 where
  getAbilities (BackIntoTheDepthsV2 a) =
    extend
      a
      [ restricted
          a
          1
          ( EachUndefeatedInvestigator (at_ $ locationIs Locations.gatewayToYhanthlei)
              <> foldMap (exists . InvestigatorWithKey) [BlueKey, RedKey, YellowKey, GreenKey]
              <> DuringTurn Anyone
          )
          $ Objective
          $ FastAbility Free
      ]

instance RunMessage BackIntoTheDepthsV2 where
  runMessage msg a@(BackIntoTheDepthsV2 attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      selectEach (not_ $ locationIs Locations.gatewayToYhanthlei) removeLocation
      n <- getPlayerCount
      lairOfDagonCard <- getSetAsideCard Locations.lairOfDagon
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
      -- v2: both copies of Stirring in Their Sleep join the encounter deck.
      shuffleSetAsideIntoEncounterDeck [HBTreacheries.stirringInTheirSleep]

      advanceActDeck attrs
      pure a
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure a
    _ -> BackIntoTheDepthsV2 <$> liftRunMessage msg attrs
