module Arkham.Act.Cards.RecoverTheRelic
  ( RecoverTheRelic(..)
  , recoverTheRelic
  ) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes

newtype RecoverTheRelic = RecoverTheRelic ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

recoverTheRelic :: ActCard RecoverTheRelic
recoverTheRelic = act (3, A) RecoverTheRelic Cards.recoverTheRelic Nothing

instance RunMessage RecoverTheRelic where
  runMessage msg (RecoverTheRelic attrs) =
    RecoverTheRelic <$> runMessage msg attrs
