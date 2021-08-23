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
import Arkham.Types.Message

newtype AccademiaBridge = AccademiaBridge LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

accademiaBridge :: LocationCard AccademiaBridge
accademiaBridge = locationWith
  AccademiaBridge
  Cards.accademiaBridge
  2
  (PerPlayer 1)
  NoSymbol
  []
  (connectsToL .~ singleton RightOf)

instance HasModifiersFor env AccademiaBridge

instance HasAbilities env AccademiaBridge where
  getAbilities iid window (AccademiaBridge attrs) = getAbilities iid window attrs

instance LocationRunner env => RunMessage env AccademiaBridge where
  runMessage msg l@(AccademiaBridge attrs) = case msg of
    MoveFrom iid lid | lid == toId attrs -> l <$ push (LoseResources iid 2)
    _ -> AccademiaBridge <$> runMessage msg attrs
