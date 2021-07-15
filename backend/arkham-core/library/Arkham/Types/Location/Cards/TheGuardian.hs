module Arkham.Types.Location.Cards.TheGuardian
  ( theGuardian
  , TheGuardian(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Direction
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationSymbol

newtype TheGuardian = TheGuardian LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theGuardian :: LocationCard TheGuardian
theGuardian = locationWith
  TheGuardian
  Cards.theGuardian
  0
  (Static 0)
  NoSymbol
  []
  (connectsToL .~ singleton RightOf)

instance HasModifiersFor env TheGuardian

instance ActionRunner env => HasActions env TheGuardian where
  getActions iid window (TheGuardian attrs) = getActions iid window attrs

instance LocationRunner env => RunMessage env TheGuardian where
  runMessage msg (TheGuardian attrs) = TheGuardian <$> runMessage msg attrs
