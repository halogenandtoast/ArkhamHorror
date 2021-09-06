module Arkham.Types.Act.Cards.DiscoveringTheTruth
  ( DiscoveringTheTruth(..)
  , discoveringTheTruth
  ) where

import Arkham.Prelude

import qualified Arkham.Act.Cards as Cards
import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Runner
import Arkham.Types.Classes

newtype DiscoveringTheTruth = DiscoveringTheTruth ActAttrs
  deriving anyclass (IsAct, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

discoveringTheTruth :: ActCard DiscoveringTheTruth
discoveringTheTruth =
  act (1, A) DiscoveringTheTruth Cards.discoveringTheTruth Nothing

instance ActRunner env => RunMessage env DiscoveringTheTruth where
  runMessage msg (DiscoveringTheTruth attrs) =
    DiscoveringTheTruth <$> runMessage msg attrs
