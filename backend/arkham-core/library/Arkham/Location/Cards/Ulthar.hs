module Arkham.Location.Cards.Ulthar ( ulthar , Ulthar(..)) where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype Ulthar = Ulthar LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ulthar :: LocationCard Ulthar
ulthar = location Ulthar Cards.ulthar 1 (PerPlayer 1)

instance HasAbilities Ulthar where
  getAbilities (Ulthar attrs) =
    getAbilities attrs
    -- withRevealedAbilities attrs []

instance RunMessage Ulthar where
  runMessage msg (Ulthar attrs) =
    Ulthar <$> runMessage msg attrs
