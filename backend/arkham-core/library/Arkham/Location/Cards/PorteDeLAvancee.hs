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
porteDeLAvancee = location PorteDeLAvancee Cards.porteDeLAvancee 3 (PerPlayer 1) Circle [Squiggle]

instance HasAbilities PorteDeLAvancee where
  getAbilities (PorteDeLAvancee attrs) =
    getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage PorteDeLAvancee where
  runMessage msg (PorteDeLAvancee attrs) =
    PorteDeLAvancee <$> runMessage msg attrs
