module Arkham.Act.Cards.Repossession
  ( Repossession(..)
  , repossession
  ) where

import Arkham.Prelude

import qualified Arkham.Act.Cards as Cards
import Arkham.Act.Runner
import Arkham.Classes

newtype Repossession = Repossession ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

repossession :: ActCard Repossession
repossession = act (3, A) Repossession Cards.repossession Nothing

instance RunMessage Repossession where
  runMessage msg (Repossession attrs) = Repossession <$> runMessage msg attrs
