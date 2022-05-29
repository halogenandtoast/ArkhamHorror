module Arkham.Location.Cards.TombOfShadows
  ( tombOfShadows
  , TombOfShadows(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Runner

newtype TombOfShadows = TombOfShadows LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

tombOfShadows :: LocationCard TombOfShadows
tombOfShadows = location TombOfShadows Cards.tombOfShadows 0 (Static 0) NoSymbol []

instance HasAbilities TombOfShadows where
  getAbilities (TombOfShadows attrs) =
    getAbilities attrs
    -- withBaseAbilities attrs []

instance LocationRunner env => RunMessage env TombOfShadows where
  runMessage msg (TombOfShadows attrs) =
    TombOfShadows <$> runMessage msg attrs
