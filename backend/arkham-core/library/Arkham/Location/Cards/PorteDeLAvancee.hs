module Arkham.Location.Cards.PorteDeLAvancee
  ( porteDeLAvancee
  , PorteDeLAvancee(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Runner

newtype PorteDeLAvancee = PorteDeLAvancee LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

porteDeLAvancee :: LocationCard PorteDeLAvancee
porteDeLAvancee = location PorteDeLAvancee Cards.porteDeLAvancee 0 (Static 0) NoSymbol []

instance HasAbilities PorteDeLAvancee where
  getAbilities (PorteDeLAvancee attrs) =
    getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage PorteDeLAvancee where
  runMessage msg (PorteDeLAvancee attrs) =
    PorteDeLAvancee <$> runMessage msg attrs
