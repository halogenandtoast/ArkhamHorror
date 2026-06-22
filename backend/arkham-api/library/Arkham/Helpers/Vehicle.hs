module Arkham.Helpers.Vehicle (module Arkham.Helpers.Vehicle, module X) where

import Arkham.Ability
import Arkham.Asset.Types (AssetAttrs)
import Arkham.Card.CardCode
import Arkham.Classes.Entity
import Arkham.Classes.Query
import Arkham.Constants as X (pattern VehicleEnterExitAbility)
import Arkham.Helpers.Location
import Arkham.Id
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted
import Arkham.Message.Lifted.Placement
import Arkham.Prelude
import Arkham.Projection
import Arkham.Source
import Arkham.Window qualified as Window
import Data.UUID qualified as UUID

vehicleEnterOrExitAbility :: (Sourceable a, HasCardCode a) => a -> Ability
vehicleEnterOrExitAbility x =
  playerLimit PerRound
    $ restricted x VehicleEnterExitAbility (oneOf [CanEnterThisVehicle, CanLeaveThisVehicle])
    $ FastAbility Free

enterOrExitVehicle
  :: (ReverseQueue m, Entity a, EntityAttrs a ~ AssetAttrs) => InvestigatorId -> a -> m a
enterOrExitVehicle iid a = do
  field InvestigatorPlacement iid >>= \case
    AtLocation _ -> place iid (InVehicle (toAttrs a).id)
    p@(InVehicle _) ->
      placementLocation p >>= \case
        Nothing -> error "No location for vehicle"
        Just lid -> place iid (AtLocation lid)
    _ -> error "Invalid placement"
  pure a

moveVehicle :: (ToId asset AssetId, ReverseQueue m) => asset -> LocationId -> LocationId -> m ()
moveVehicle (asId -> asset) fromLid toLid = do
  iids <- select $ InVehicleMatching (AssetWithId asset)
  -- Carried investigators move along with the vehicle, so fire the "moves"
  -- window in addition to leaving/entering. Without this, movement reactions
  -- (e.g. Ursula Downs' "after you move, investigate") never trigger for
  -- passengers. See #4883. No Movement entity backs a vehicle move, so use a
  -- nil MovementId (matching the fallback in Arkham.Window).
  let source = AssetSource asset
      movementId = MovementId UUID.nil
  checkWindows
    $ [Window.mkWhen (Window.Leaving iid' fromLid) | iid' <- iids]
    <> [Window.mkWhen (Window.Moves iid' source (Just fromLid) toLid movementId) | iid' <- iids]
  place asset toLid
  -- Snapshot enemy presence at entry so "after you enter a location with 1+
  -- enemies" triggers fire for carried investigators too. See #4813.
  toLidHasEnemy <- selectAny (enemyAt toLid)
  checkWindows
    $ [Window.mkAfter (Window.Entering iid' toLid) | iid' <- iids]
    <> [Window.mkAfter (Window.EnteringLocationWithEnemy iid' toLid) | toLidHasEnemy, iid' <- iids]
    <> [Window.mkAfter (Window.Moves iid' source (Just fromLid) toLid movementId) | iid' <- iids]
