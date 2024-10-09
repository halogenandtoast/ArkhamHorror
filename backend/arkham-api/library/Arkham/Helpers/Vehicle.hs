module Arkham.Helpers.Vehicle (module Arkham.Helpers.Vehicle, module X) where

import Arkham.Ability
import Arkham.Asset.Types (AssetAttrs)
import Arkham.Card.CardCode
import Arkham.Classes.Entity
import Arkham.Constants as X (pattern VehicleEnterExitAbility)
import Arkham.Helpers.Location
import Arkham.Id
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted
import Arkham.Message.Lifted.Placement
import Arkham.Placement
import Arkham.Prelude
import Arkham.Projection
import Arkham.Source

vehicleEnterOrExitAbility :: (Sourceable a, HasCardCode a) => a -> Ability
vehicleEnterOrExitAbility x =
  playerLimit PerRound
    $ restricted x VehicleEnterExitAbility (oneOf [CanEnterThisVehicle, CanLeaveThisVehicle])
    $ FastAbility Free

enterOrExitVehicle
  :: (ReverseQueue m, Entity a, EntityAttrs a ~ AssetAttrs) => InvestigatorId -> a -> m a
enterOrExitVehicle iid a = do
  field InvestigatorPlacement iid >>= \case
    AtLocation _ -> place iid (InVehicle $ (toAttrs a).id)
    p@(InVehicle _) ->
      placementLocation p >>= \case
        Nothing -> error "No location for vehicle"
        Just lid -> place iid (AtLocation lid)
    _ -> error "Invalid placement"
  pure a
