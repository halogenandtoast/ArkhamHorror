module Arkham.Location.Cards.UnmarkedTomb
  ( unmarkedTomb
  , UnmarkedTomb(..)
  )
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype UnmarkedTomb = UnmarkedTomb LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unmarkedTomb :: LocationCard UnmarkedTomb
unmarkedTomb = location UnmarkedTomb Cards.unmarkedTomb 0 (Static 0)

instance HasAbilities UnmarkedTomb where
  getAbilities (UnmarkedTomb attrs) =
    getAbilities attrs
    -- withRevealedAbilities attrs []

instance RunMessage UnmarkedTomb where
  runMessage msg (UnmarkedTomb attrs) =
    UnmarkedTomb <$> runMessage msg attrs
