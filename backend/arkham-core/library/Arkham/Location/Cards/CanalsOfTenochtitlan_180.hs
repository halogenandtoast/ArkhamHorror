module Arkham.Location.Cards.CanalsOfTenochtitlan_180
  ( canalsOfTenochtitlan_180
  , CanalsOfTenochtitlan_180(..)
  ) where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype CanalsOfTenochtitlan_180 = CanalsOfTenochtitlan_180 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

canalsOfTenochtitlan_180 :: LocationCard CanalsOfTenochtitlan_180
canalsOfTenochtitlan_180 =
  location CanalsOfTenochtitlan_180 Cards.canalsOfTenochtitlan_180 5 (PerPlayer 1)

instance HasAbilities CanalsOfTenochtitlan_180 where
  getAbilities (CanalsOfTenochtitlan_180 attrs) = getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage CanalsOfTenochtitlan_180 where
  runMessage msg (CanalsOfTenochtitlan_180 attrs) =
    CanalsOfTenochtitlan_180 <$> runMessage msg attrs
