module Arkham.Act.Cards.RestrictedAccess
  ( RestrictedAccess(..)
  , restrictedAccess
  ) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes

newtype RestrictedAccess = RestrictedAccess ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

restrictedAccess :: ActCard RestrictedAccess
restrictedAccess = act (2, A) RestrictedAccess Cards.restrictedAccess Nothing

instance RunMessage RestrictedAccess where
  runMessage msg (RestrictedAccess attrs) =
    RestrictedAccess <$> runMessage msg attrs
