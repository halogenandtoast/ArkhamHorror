module Arkham.Act.Cards.EscapeTheRuins
  ( EscapeTheRuins(..)
  , escapeTheRuins
  ) where

import Arkham.Prelude

import qualified Arkham.Act.Cards as Cards
import Arkham.Act.Runner
import Arkham.Classes

newtype EscapeTheRuins = EscapeTheRuins ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

escapeTheRuins :: ActCard EscapeTheRuins
escapeTheRuins = act (3, A) EscapeTheRuins Cards.escapeTheRuins Nothing

instance RunMessage EscapeTheRuins where
  runMessage msg (EscapeTheRuins attrs) = EscapeTheRuins <$> runMessage msg attrs
