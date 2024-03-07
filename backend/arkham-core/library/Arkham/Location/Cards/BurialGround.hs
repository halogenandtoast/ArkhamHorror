module Arkham.Location.Cards.BurialGround
  ( burialGround
  , BurialGround(..)
  )
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype BurialGround = BurialGround LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

burialGround :: LocationCard BurialGround
burialGround = location BurialGround Cards.burialGround 0 (Static 0)

instance HasAbilities BurialGround where
  getAbilities (BurialGround attrs) =
    getAbilities attrs
    -- withRevealedAbilities attrs []

instance RunMessage BurialGround where
  runMessage msg (BurialGround attrs) =
    BurialGround <$> runMessage msg attrs
