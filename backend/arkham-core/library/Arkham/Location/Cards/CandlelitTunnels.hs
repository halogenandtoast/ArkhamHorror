module Arkham.Location.Cards.CandlelitTunnels
  ( candlelitTunnels
  , CandlelitTunnels(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Runner

newtype CandlelitTunnels = CandlelitTunnels LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

candlelitTunnels :: LocationCard CandlelitTunnels
candlelitTunnels = location CandlelitTunnels Cards.candlelitTunnels 0 (Static 0) NoSymbol []

instance HasAbilities CandlelitTunnels where
  getAbilities (CandlelitTunnels attrs) =
    getAbilities attrs
    -- withBaseAbilities attrs []

instance LocationRunner env => RunMessage env CandlelitTunnels where
  runMessage msg (CandlelitTunnels attrs) =
    CandlelitTunnels <$> runMessage msg attrs
