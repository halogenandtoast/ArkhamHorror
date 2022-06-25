module Arkham.Act.Cards.InLostCarcosa
  ( InLostCarcosa(..)
  , inLostCarcosa
  ) where

import Arkham.Prelude

import qualified Arkham.Act.Cards as Cards
import Arkham.Act.Runner
import Arkham.Classes

newtype InLostCarcosa = InLostCarcosa ActAttrs
  deriving anyclass (IsAct, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

inLostCarcosa :: ActCard InLostCarcosa
inLostCarcosa = act (1, A) InLostCarcosa Cards.inLostCarcosa Nothing

instance RunMessage InLostCarcosa where
  runMessage msg (InLostCarcosa attrs) = InLostCarcosa <$> runMessage msg attrs
