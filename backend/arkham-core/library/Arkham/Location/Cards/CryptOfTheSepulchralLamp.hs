module Arkham.Location.Cards.CryptOfTheSepulchralLamp
  ( cryptOfTheSepulchralLamp
  , CryptOfTheSepulchralLamp(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Runner

newtype CryptOfTheSepulchralLamp = CryptOfTheSepulchralLamp LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cryptOfTheSepulchralLamp :: LocationCard CryptOfTheSepulchralLamp
cryptOfTheSepulchralLamp = location CryptOfTheSepulchralLamp Cards.cryptOfTheSepulchralLamp 0 (Static 0) NoSymbol []

instance HasAbilities CryptOfTheSepulchralLamp where
  getAbilities (CryptOfTheSepulchralLamp attrs) =
    getAbilities attrs
    -- withBaseAbilities attrs []

instance LocationRunner env => RunMessage env CryptOfTheSepulchralLamp where
  runMessage msg (CryptOfTheSepulchralLamp attrs) =
    CryptOfTheSepulchralLamp <$> runMessage msg attrs
