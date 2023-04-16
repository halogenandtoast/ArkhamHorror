module Arkham.Location.Cards.KeziahsRoom
  ( keziahsRoom
  , KeziahsRoom(..)
  ) where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype KeziahsRoom = KeziahsRoom LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

keziahsRoom :: LocationCard KeziahsRoom
keziahsRoom = location KeziahsRoom Cards.keziahsRoom 3 (Static 0)

instance HasAbilities KeziahsRoom where
  getAbilities (KeziahsRoom attrs) = getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage KeziahsRoom where
  runMessage msg (KeziahsRoom attrs) = KeziahsRoom <$> runMessage msg attrs
