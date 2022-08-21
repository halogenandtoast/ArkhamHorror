module Arkham.Location.Cards.SecretPassage
  ( secretPassage
  , SecretPassage(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards
import Arkham.GameValue
import Arkham.Location.Runner

newtype SecretPassage = SecretPassage LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

secretPassage :: LocationCard SecretPassage
secretPassage = location SecretPassage Cards.secretPassage 5 (PerPlayer 1)

instance HasAbilities SecretPassage where
  getAbilities (SecretPassage attrs) =
    getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage SecretPassage where
  runMessage msg (SecretPassage attrs) =
    SecretPassage <$> runMessage msg attrs
