module Arkham.Location.Cards.LonelyIsle (lonelyIsle, LonelyIsle (..)) where

import Arkham.Ability
import Arkham.Helpers.Scenario
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Grid
import Arkham.Location.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenario.Deck

newtype LonelyIsle = LonelyIsle LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lonelyIsle :: LocationCard LonelyIsle
lonelyIsle = locationWith LonelyIsle Cards.lonelyIsle 5 (Static 0) connectsToAdjacent

instance HasAbilities LonelyIsle where
  getAbilities (LonelyIsle a) =
    extendRevealed a [mkAbility a 1 $ forced $ RevealLocation #after Anyone (be a)]

instance RunMessage LonelyIsle where
  runMessage msg l@(LonelyIsle attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      grid <- getGrid
      tunnels <- take 2 <$> getScenarioDeck TidalTunnelDeck

      let
        positions = case findInGrid attrs.id grid of
          Just (Pos 0 3) -> [Pos (-1) 3, Pos 1 3]
          Just (Pos 4 2) -> [Pos 5 2, Pos 4 1]
          Just (Pos 4 (-2)) -> [Pos 5 (-2), Pos 4 (-1)]
          Just (Pos (-4) 2) -> [Pos (-5) 2, Pos (-4) 1]
          Just (Pos (-4) (-2)) -> [Pos (-5) (-2), Pos (-4) (-1)]
          _ -> error "invalid location"
      zipWithM_ placeLocationInGrid positions tunnels
      pure l
    _ -> LonelyIsle <$> liftRunMessage msg attrs
