module Arkham.Location.Cards.PrivateRoom
  ( privateRoom
  , PrivateRoom(..)
  )
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype PrivateRoom = PrivateRoom LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

privateRoom :: LocationCard PrivateRoom
privateRoom = location PrivateRoom Cards.privateRoom 4 (Static 0)

instance HasAbilities PrivateRoom where
  getAbilities (PrivateRoom attrs) =
    getAbilities attrs
    -- withRevealedAbilities attrs []

instance RunMessage PrivateRoom where
  runMessage msg (PrivateRoom attrs) =
    PrivateRoom <$> runMessage msg attrs
