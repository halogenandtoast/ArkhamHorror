module Arkham.Location.Cards.PereLachaiseCemetery
  ( pereLachaiseCemetery
  , PereLachaiseCemetery(..)
  ) where

import Arkham.Prelude

import Arkham.Location.Cards qualified as Cards
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Runner

newtype PereLachaiseCemetery = PereLachaiseCemetery LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

pereLachaiseCemetery :: LocationCard PereLachaiseCemetery
pereLachaiseCemetery = location
  PereLachaiseCemetery
  Cards.pereLachaiseCemetery
  1
  (PerPlayer 2)
  T
  [Equals, Moon]

instance HasAbilities PereLachaiseCemetery where
  getAbilities (PereLachaiseCemetery attrs) = getAbilities attrs

instance LocationRunner env => RunMessage env PereLachaiseCemetery where
  runMessage msg (PereLachaiseCemetery attrs) =
    PereLachaiseCemetery <$> runMessage msg attrs
