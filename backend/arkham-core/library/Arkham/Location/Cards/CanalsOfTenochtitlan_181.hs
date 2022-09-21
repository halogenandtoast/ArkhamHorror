module Arkham.Location.Cards.CanalsOfTenochtitlan_181
  ( canalsOfTenochtitlan_181
  , CanalsOfTenochtitlan_181(..)
  ) where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype CanalsOfTenochtitlan_181 = CanalsOfTenochtitlan_181 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

canalsOfTenochtitlan_181 :: LocationCard CanalsOfTenochtitlan_181
canalsOfTenochtitlan_181 =
  location CanalsOfTenochtitlan_181 Cards.canalsOfTenochtitlan_181 2 (PerPlayer 1)

instance HasAbilities CanalsOfTenochtitlan_181 where
  getAbilities (CanalsOfTenochtitlan_181 attrs) = getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage CanalsOfTenochtitlan_181 where
  runMessage msg (CanalsOfTenochtitlan_181 attrs) =
    CanalsOfTenochtitlan_181 <$> runMessage msg attrs
