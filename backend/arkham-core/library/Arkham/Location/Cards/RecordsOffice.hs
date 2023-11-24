module Arkham.Location.Cards.RecordsOffice
  ( recordsOffice
  , RecordsOffice(..)
  )
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype RecordsOffice = RecordsOffice LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

recordsOffice :: LocationCard RecordsOffice
recordsOffice = location RecordsOffice Cards.recordsOffice 3 (PerPlayer 2)

instance HasAbilities RecordsOffice where
  getAbilities (RecordsOffice attrs) =
    getAbilities attrs
    -- withRevealedAbilities attrs []

instance RunMessage RecordsOffice where
  runMessage msg (RecordsOffice attrs) =
    RecordsOffice <$> runMessage msg attrs
