module Arkham.Location.Cards.BlockedPassage
  ( blockedPassage
  , BlockedPassage(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Runner

newtype BlockedPassage = BlockedPassage LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

blockedPassage :: LocationCard BlockedPassage
blockedPassage = location BlockedPassage Cards.blockedPassage 0 (Static 0) NoSymbol []

instance HasAbilities BlockedPassage where
  getAbilities (BlockedPassage attrs) =
    getAbilities attrs
    -- withBaseAbilities attrs []

instance LocationRunner env => RunMessage env BlockedPassage where
  runMessage msg (BlockedPassage attrs) =
    BlockedPassage <$> runMessage msg attrs
