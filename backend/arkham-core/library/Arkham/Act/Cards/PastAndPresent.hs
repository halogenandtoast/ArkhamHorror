module Arkham.Act.Cards.PastAndPresent
  ( PastAndPresent(..)
  , pastAndPresent
  ) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes

newtype PastAndPresent = PastAndPresent ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

pastAndPresent :: ActCard PastAndPresent
pastAndPresent = act (2, A) PastAndPresent Cards.pastAndPresent Nothing

instance RunMessage PastAndPresent where
  runMessage msg (PastAndPresent attrs) =
    PastAndPresent <$> runMessage msg attrs
