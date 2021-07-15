module Arkham.Types.Location.Cards.VenetianGarden
  ( venetianGarden
  , VenetianGarden(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Direction
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationSymbol

newtype VenetianGarden = VenetianGarden LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

venetianGarden :: LocationCard VenetianGarden
venetianGarden = locationWith
  VenetianGarden
  Cards.venetianGarden
  0
  (Static 0)
  NoSymbol
  []
  (connectsToL .~ singleton RightOf)

instance HasModifiersFor env VenetianGarden

instance ActionRunner env => HasActions env VenetianGarden where
  getActions iid window (VenetianGarden attrs) = getActions iid window attrs

instance LocationRunner env => RunMessage env VenetianGarden where
  runMessage msg (VenetianGarden attrs) =
    VenetianGarden <$> runMessage msg attrs
