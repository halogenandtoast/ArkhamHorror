module Arkham.Location.Cards.Uptown_297
  ( uptown_297
  , Uptown_297(..)
  )
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype Uptown_297 = Uptown_297 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

uptown_297 :: LocationCard Uptown_297
uptown_297 = location Uptown_297 Cards.uptown_297 4 (Static 0)

instance HasAbilities Uptown_297 where
  getAbilities (Uptown_297 attrs) =
    getAbilities attrs
    -- withRevealedAbilities attrs []

instance RunMessage Uptown_297 where
  runMessage msg (Uptown_297 attrs) =
    Uptown_297 <$> runMessage msg attrs
