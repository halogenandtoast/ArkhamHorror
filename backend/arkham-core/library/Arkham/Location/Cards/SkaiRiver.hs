module Arkham.Location.Cards.SkaiRiver
  ( skaiRiver
  , SkaiRiver(..)
  )
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype SkaiRiver = SkaiRiver LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

skaiRiver :: LocationCard SkaiRiver
skaiRiver = location SkaiRiver Cards.skaiRiver 2 (Static 0)

instance HasAbilities SkaiRiver where
  getAbilities (SkaiRiver attrs) =
    getAbilities attrs
    -- withRevealedAbilities attrs []

instance RunMessage SkaiRiver where
  runMessage msg (SkaiRiver attrs) =
    SkaiRiver <$> runMessage msg attrs
