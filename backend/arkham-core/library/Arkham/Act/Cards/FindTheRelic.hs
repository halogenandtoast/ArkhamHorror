module Arkham.Act.Cards.FindTheRelic
  ( FindTheRelic(..)
  , findTheRelic
  ) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes

newtype FindTheRelic = FindTheRelic ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

findTheRelic :: ActCard FindTheRelic
findTheRelic = act (3, A) FindTheRelic Cards.findTheRelic Nothing

instance RunMessage FindTheRelic where
  runMessage msg (FindTheRelic attrs) = FindTheRelic <$> runMessage msg attrs
