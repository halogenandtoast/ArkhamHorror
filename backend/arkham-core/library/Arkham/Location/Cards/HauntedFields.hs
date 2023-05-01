module Arkham.Location.Cards.HauntedFields
  ( hauntedFields
  , HauntedFields(..)
  )
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype HauntedFields = HauntedFields LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hauntedFields :: LocationCard HauntedFields
hauntedFields = location HauntedFields Cards.hauntedFields 3 (PerPlayer 2)

instance HasAbilities HauntedFields where
  getAbilities (HauntedFields attrs) =
    getAbilities attrs
    -- withRevealedAbilities attrs []

instance RunMessage HauntedFields where
  runMessage msg (HauntedFields attrs) =
    HauntedFields <$> runMessage msg attrs
