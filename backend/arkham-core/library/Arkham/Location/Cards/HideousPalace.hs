module Arkham.Location.Cards.HideousPalace
  ( hideousPalace
  , HideousPalace(..)
  )
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype HideousPalace = HideousPalace LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hideousPalace :: LocationCard HideousPalace
hideousPalace = location HideousPalace Cards.hideousPalace 3 (Static 4)

instance HasAbilities HideousPalace where
  getAbilities (HideousPalace attrs) =
    getAbilities attrs
    -- withRevealedAbilities attrs []

instance RunMessage HideousPalace where
  runMessage msg (HideousPalace attrs) =
    HideousPalace <$> runMessage msg attrs
