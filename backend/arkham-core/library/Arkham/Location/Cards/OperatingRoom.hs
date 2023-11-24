module Arkham.Location.Cards.OperatingRoom
  ( operatingRoom
  , OperatingRoom(..)
  )
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype OperatingRoom = OperatingRoom LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

operatingRoom :: LocationCard OperatingRoom
operatingRoom = location OperatingRoom Cards.operatingRoom 2 (PerPlayer 1)

instance HasAbilities OperatingRoom where
  getAbilities (OperatingRoom attrs) =
    getAbilities attrs
    -- withRevealedAbilities attrs []

instance RunMessage OperatingRoom where
  runMessage msg (OperatingRoom attrs) =
    OperatingRoom <$> runMessage msg attrs
