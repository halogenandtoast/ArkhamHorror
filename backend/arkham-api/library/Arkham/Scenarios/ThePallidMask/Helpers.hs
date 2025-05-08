module Arkham.Scenarios.ThePallidMask.Helpers where

import Arkham.Campaigns.ThePathToCarcosa.Helpers
import Arkham.Card
import Arkham.Classes
import Arkham.Classes.HasGame
import Arkham.Direction
import Arkham.Helpers.Message hiding (Label)
import Arkham.Helpers.Scenario (getGrid)
import Arkham.I18n
import Arkham.Id
import Arkham.Location.Grid (Pos (..))
import Arkham.Location.Grid qualified as Grid
import Arkham.Location.Types
import Arkham.Matcher.Location
import Arkham.Message.Lifted.Queue
import Arkham.Prelude
import Control.Monad (zipWithM)

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = campaignI18n $ scope "thePallidMask" a

getStartingLocation :: HasGame m => m LocationId
getStartingLocation = selectJust $ LocationInPosition (Pos 0 0)

directionEmpty :: HasGame m => LocationAttrs -> Direction -> m Bool
directionEmpty attrs dir = case attrs.position of
  Nothing -> pure False
  Just pos -> do
    grid <- getGrid
    let pos' = Grid.updatePosition pos (toGridDirection dir)
    pure $ Grid.isEmpty pos' grid

toMaybePlacement
  :: (MonadRandom m, HasGame m) => LocationAttrs -> Direction -> m (Maybe (Card -> m Message))
toMaybePlacement attrs dir = runMaybeT do
  pos <- hoistMaybe attrs.position
  grid <- getGrid
  let pos' = Grid.updatePosition pos (toGridDirection dir)
  guard $ Grid.isEmpty pos' grid
  pure $ fmap snd . placeLocationInGrid pos'

placeDrawnLocations
  :: (MonadRandom m, HasQueue Message m, HasGame m) => LocationAttrs -> [Card] -> [Direction] -> m ()
placeDrawnLocations attrs cards directions = do
  placements <- mapMaybeM (toMaybePlacement attrs) directions
  msgs <- zipWithM ($) placements cards
  pushAll msgs

placeAtDirection_ :: ReverseQueue m => Direction -> LocationAttrs -> Card -> m ()
placeAtDirection_ direction attrs card = void $ placeAtDirection direction attrs card

placeAtDirection :: ReverseQueue m => Direction -> LocationAttrs -> Card -> m LocationId
placeAtDirection direction attrs card = do
  case attrs.position of
    Nothing -> error "Missing position"
    Just pos -> do
      let pos' = Grid.updatePosition pos (toGridDirection direction)
      (lid, msg) <- placeLocationInGrid pos' card
      push msg
      pure lid
