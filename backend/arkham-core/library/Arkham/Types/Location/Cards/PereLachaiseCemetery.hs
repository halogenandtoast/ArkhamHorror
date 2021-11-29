module Arkham.Types.Location.Cards.PereLachaiseCemetery
  ( pereLachaiseCemetery
  , PereLachaiseCemetery(..)
  ) where

import Arkham.Prelude

import Arkham.Location.Cards qualified as Cards
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs

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
