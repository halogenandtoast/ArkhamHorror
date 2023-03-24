module Arkham.Act.Cards.EscapeTheCage
  ( EscapeTheCage(..)
  , escapeTheCage
  ) where

import Arkham.Prelude

import qualified Arkham.Act.Cards as Cards
import Arkham.Act.Runner
import Arkham.Classes

newtype EscapeTheCage = EscapeTheCage ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

escapeTheCage :: ActCard EscapeTheCage
escapeTheCage = act (3, A) EscapeTheCage Cards.escapeTheCage Nothing

instance RunMessage EscapeTheCage where
  runMessage msg (EscapeTheCage attrs) = EscapeTheCage <$> runMessage msg attrs
