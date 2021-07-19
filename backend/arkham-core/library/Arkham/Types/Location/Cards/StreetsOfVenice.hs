module Arkham.Types.Location.Cards.StreetsOfVenice
  ( streetsOfVenice
  , StreetsOfVenice(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Direction
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Window

newtype StreetsOfVenice = StreetsOfVenice LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

streetsOfVenice :: LocationCard StreetsOfVenice
streetsOfVenice = locationWith
  StreetsOfVenice
  Cards.streetsOfVenice
  2
  (Static 2)
  NoSymbol
  []
  (connectsToL .~ singleton RightOf)

instance HasModifiersFor env StreetsOfVenice

instance ActionRunner env => HasActions env StreetsOfVenice where
  getActions iid FastPlayerWindow (StreetsOfVenice attrs) =
    pure [locationAbility iid $ mkAbility attrs 1 (FastAbility Free)]
  getActions iid window (StreetsOfVenice attrs) = getActions iid window attrs

instance LocationRunner env => RunMessage env StreetsOfVenice where
  runMessage msg l@(StreetsOfVenice attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      case setToList (locationConnectedLocations attrs) of
        [] -> error "No connections?"
        (x : _) -> l <$ push (MoveAction iid x Free False)
    _ -> StreetsOfVenice <$> runMessage msg attrs
