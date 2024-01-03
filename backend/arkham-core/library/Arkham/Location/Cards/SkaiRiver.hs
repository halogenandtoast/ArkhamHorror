module Arkham.Location.Cards.SkaiRiver (skaiRiver, SkaiRiver (..)) where

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Prelude

newtype SkaiRiver = SkaiRiver LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

skaiRiver :: LocationCard SkaiRiver
skaiRiver = location SkaiRiver Cards.skaiRiver 2 (Static 0)

instance HasAbilities SkaiRiver where
  getAbilities (SkaiRiver attrs) =
    withRevealedAbilities
      attrs
      [mkAbility attrs 1 $ forced $ Leaves #when You $ LocationWithId (toId attrs)]

instance RunMessage SkaiRiver where
  runMessage msg (SkaiRiver attrs) =
    SkaiRiver <$> runMessage msg attrs
