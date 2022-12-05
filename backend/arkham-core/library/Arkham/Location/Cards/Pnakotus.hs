module Arkham.Location.Cards.Pnakotus
  ( pnakotus
  , Pnakotus(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards
import Arkham.GameValue
import Arkham.Location.Runner

newtype Pnakotus = Pnakotus LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

pnakotus :: LocationCard Pnakotus
pnakotus = location Pnakotus Cards.pnakotus 2 (Static 3)

instance HasAbilities Pnakotus where
  getAbilities (Pnakotus attrs) =
    getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage Pnakotus where
  runMessage msg (Pnakotus attrs) =
    Pnakotus <$> runMessage msg attrs
