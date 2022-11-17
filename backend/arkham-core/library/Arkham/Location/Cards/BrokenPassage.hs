module Arkham.Location.Cards.BrokenPassage
  ( brokenPassage
  , BrokenPassage(..)
  ) where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype BrokenPassage = BrokenPassage LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

brokenPassage :: LocationCard BrokenPassage
brokenPassage =
  symbolLabel $ location BrokenPassage Cards.brokenPassage 3 (Static 0)

instance HasAbilities BrokenPassage where
  getAbilities (BrokenPassage attrs) = getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage BrokenPassage where
  runMessage msg (BrokenPassage attrs) = BrokenPassage <$> runMessage msg attrs
