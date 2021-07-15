module Arkham.Types.Location.Cards.AccademiaBridge
  ( accademiaBridge
  , AccademiaBridge(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Direction
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationSymbol

newtype AccademiaBridge = AccademiaBridge LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

accademiaBridge :: LocationCard AccademiaBridge
accademiaBridge = locationWith
  AccademiaBridge
  Cards.accademiaBridge
  0
  (Static 0)
  NoSymbol
  []
  (connectsToL .~ singleton RightOf)

instance HasModifiersFor env AccademiaBridge

instance ActionRunner env => HasActions env AccademiaBridge where
  getActions iid window (AccademiaBridge attrs) = getActions iid window attrs

instance LocationRunner env => RunMessage env AccademiaBridge where
  runMessage msg (AccademiaBridge attrs) =
    AccademiaBridge <$> runMessage msg attrs
