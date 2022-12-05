module Arkham.Act.Cards.WorldsBeyond
  ( WorldsBeyond(..)
  , worldsBeyond
  ) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Matcher

newtype WorldsBeyond = WorldsBeyond ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

worldsBeyond :: ActCard WorldsBeyond
worldsBeyond = act
  (1, A)
  WorldsBeyond
  Cards.worldsBeyond
  (Just $ GroupClueCost (PerPlayer 2) Anywhere)

instance RunMessage WorldsBeyond where
  runMessage msg (WorldsBeyond attrs) = WorldsBeyond <$> runMessage msg attrs
