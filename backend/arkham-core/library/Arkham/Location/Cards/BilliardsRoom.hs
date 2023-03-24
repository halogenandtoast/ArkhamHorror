module Arkham.Location.Cards.BilliardsRoom
  ( billiardsRoom
  , BilliardsRoom(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards
import Arkham.GameValue
import Arkham.Location.Runner

newtype BilliardsRoom = BilliardsRoom LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

billiardsRoom :: LocationCard BilliardsRoom
billiardsRoom = location BilliardsRoom Cards.billiardsRoom 3 (Static 0)

instance HasAbilities BilliardsRoom where
  getAbilities (BilliardsRoom attrs) =
    getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage BilliardsRoom where
  runMessage msg (BilliardsRoom attrs) =
    BilliardsRoom <$> runMessage msg attrs
