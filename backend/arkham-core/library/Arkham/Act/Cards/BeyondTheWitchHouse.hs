module Arkham.Act.Cards.BeyondTheWitchHouse
  ( BeyondTheWitchHouse(..)
  , beyondTheWitchHouse
  ) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher

newtype BeyondTheWitchHouse = BeyondTheWitchHouse ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

beyondTheWitchHouse :: ActCard BeyondTheWitchHouse
beyondTheWitchHouse = act
  (2, A)
  BeyondTheWitchHouse
  Cards.beyondTheWitchHouse
  (Just $ GroupClueCost (PerPlayer 5) (locationIs Locations.witchHouseRuins))

instance RunMessage BeyondTheWitchHouse where
  runMessage msg (BeyondTheWitchHouse attrs) =
    BeyondTheWitchHouse <$> runMessage msg attrs
