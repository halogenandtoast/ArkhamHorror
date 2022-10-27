module Arkham.Location.Cards.GreatLibrary
  ( greatLibrary
  , GreatLibrary(..)
  ) where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype GreatLibrary = GreatLibrary LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

greatLibrary :: LocationCard GreatLibrary
greatLibrary = location GreatLibrary Cards.greatLibrary 2 (Static 4)

instance HasAbilities GreatLibrary where
  getAbilities (GreatLibrary attrs) = getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage GreatLibrary where
  runMessage msg (GreatLibrary attrs) = GreatLibrary <$> runMessage msg attrs
