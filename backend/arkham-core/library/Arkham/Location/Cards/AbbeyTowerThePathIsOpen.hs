module Arkham.Location.Cards.AbbeyTowerThePathIsOpen
  ( abbeyTowerThePathIsOpen
  , AbbeyTowerThePathIsOpen(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Runner

newtype AbbeyTowerThePathIsOpen = AbbeyTowerThePathIsOpen LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

abbeyTowerThePathIsOpen :: LocationCard AbbeyTowerThePathIsOpen
abbeyTowerThePathIsOpen = location AbbeyTowerThePathIsOpen Cards.abbeyTowerThePathIsOpen 3 (PerPlayer 2) Star [T]

instance HasAbilities AbbeyTowerThePathIsOpen where
  getAbilities (AbbeyTowerThePathIsOpen attrs) =
    getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage AbbeyTowerThePathIsOpen where
  runMessage msg (AbbeyTowerThePathIsOpen attrs) =
    AbbeyTowerThePathIsOpen <$> runMessage msg attrs
