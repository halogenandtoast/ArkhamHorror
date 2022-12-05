module Arkham.Act.Cards.IntoTheRuinsOnceAgain
  ( IntoTheRuinsOnceAgain(..)
  , intoTheRuinsOnceAgain
  ) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Matcher

newtype IntoTheRuinsOnceAgain = IntoTheRuinsOnceAgain ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

intoTheRuinsOnceAgain :: ActCard IntoTheRuinsOnceAgain
intoTheRuinsOnceAgain = act
  (1, A)
  IntoTheRuinsOnceAgain
  Cards.intoTheRuinsOnceAgain
  (Just $ GroupClueCost (PerPlayer 3) Anywhere)

instance RunMessage IntoTheRuinsOnceAgain where
  runMessage msg (IntoTheRuinsOnceAgain attrs) =
    IntoTheRuinsOnceAgain <$> runMessage msg attrs
