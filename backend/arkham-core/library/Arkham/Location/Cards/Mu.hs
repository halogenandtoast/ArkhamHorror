module Arkham.Location.Cards.Mu
  ( mu
  , Mu(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards
import Arkham.GameValue
import Arkham.Location.Runner

newtype Mu = Mu LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mu :: LocationCard Mu
mu = location Mu Cards.mu 1 (Static 4)

instance HasAbilities Mu where
  getAbilities (Mu attrs) =
    getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage Mu where
  runMessage msg (Mu attrs) =
    Mu <$> runMessage msg attrs
