module Arkham.Types.Location.Cards.SanMarcoBasilica
  ( sanMarcoBasilica
  , SanMarcoBasilica(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Direction
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationSymbol

newtype SanMarcoBasilica = SanMarcoBasilica LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sanMarcoBasilica :: LocationCard SanMarcoBasilica
sanMarcoBasilica = locationWith
  SanMarcoBasilica
  Cards.sanMarcoBasilica
  3
  (Static 0)
  NoSymbol
  []
  (connectsToL .~ singleton RightOf)

instance HasModifiersFor env SanMarcoBasilica

instance ActionRunner env => HasActions env SanMarcoBasilica where
  getActions iid window (SanMarcoBasilica attrs) = getActions iid window attrs

instance LocationRunner env => RunMessage env SanMarcoBasilica where
  runMessage msg (SanMarcoBasilica attrs) =
    SanMarcoBasilica <$> runMessage msg attrs
