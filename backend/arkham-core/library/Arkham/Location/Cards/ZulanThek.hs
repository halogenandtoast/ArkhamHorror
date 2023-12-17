module Arkham.Location.Cards.ZulanThek
  ( zulanThek
  , ZulanThek(..)
  )
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype ZulanThek = ZulanThek LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

zulanThek :: LocationCard ZulanThek
zulanThek = location ZulanThek Cards.zulanThek 4 (PerPlayer 1)

instance HasAbilities ZulanThek where
  getAbilities (ZulanThek attrs) =
    getAbilities attrs
    -- withRevealedAbilities attrs []

instance RunMessage ZulanThek where
  runMessage msg (ZulanThek attrs) =
    ZulanThek <$> runMessage msg attrs
