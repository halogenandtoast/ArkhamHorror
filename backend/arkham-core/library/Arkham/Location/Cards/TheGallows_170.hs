module Arkham.Location.Cards.TheGallows_170
  ( theGallows_170
  , TheGallows_170(..)
  )
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype TheGallows_170 = TheGallows_170 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theGallows_170 :: LocationCard TheGallows_170
theGallows_170 = location TheGallows_170 Cards.theGallows_170 0 (Static 0)

instance HasAbilities TheGallows_170 where
  getAbilities (TheGallows_170 attrs) =
    getAbilities attrs
    -- withRevealedAbilities attrs []

instance RunMessage TheGallows_170 where
  runMessage msg (TheGallows_170 attrs) =
    TheGallows_170 <$> runMessage msg attrs
