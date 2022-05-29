module Arkham.Location.Cards.StoneArchways
  ( stoneArchways
  , StoneArchways(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Runner

newtype StoneArchways = StoneArchways LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stoneArchways :: LocationCard StoneArchways
stoneArchways = location StoneArchways Cards.stoneArchways 0 (Static 0) NoSymbol []

instance HasAbilities StoneArchways where
  getAbilities (StoneArchways attrs) =
    getAbilities attrs
    -- withBaseAbilities attrs []

instance LocationRunner env => RunMessage env StoneArchways where
  runMessage msg (StoneArchways attrs) =
    StoneArchways <$> runMessage msg attrs
