module Arkham.Location.Cards.SecretPassage
  ( secretPassage
  , SecretPassage(..)
  ) where

import Arkham.Prelude

import Arkham.Direction
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype SecretPassage = SecretPassage LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

secretPassage :: LocationCard SecretPassage
secretPassage = locationWith
  SecretPassage
  Cards.secretPassage
  5
  (PerPlayer 1)
  (connectsToL .~ setFromList [LeftOf, RightOf])

instance HasAbilities SecretPassage where
  getAbilities (SecretPassage attrs) = getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage SecretPassage where
  runMessage msg (SecretPassage attrs) = SecretPassage <$> runMessage msg attrs
