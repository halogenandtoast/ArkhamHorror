module Arkham.Act.Cards.EnteringTheDreamlands
  ( EnteringTheDreamlands(..)
  , enteringTheDreamlands
  ) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes

newtype EnteringTheDreamlands = EnteringTheDreamlands ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

enteringTheDreamlands :: ActCard EnteringTheDreamlands
enteringTheDreamlands = act (1, A) EnteringTheDreamlands Cards.enteringTheDreamlands Nothing

instance RunMessage EnteringTheDreamlands where
  runMessage msg (EnteringTheDreamlands attrs) = EnteringTheDreamlands <$> runMessage msg attrs
