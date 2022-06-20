module Arkham.Location.Cards.AbbeyTowerSpiresForbidden
  ( abbeyTowerSpiresForbidden
  , AbbeyTowerSpiresForbidden(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Runner

newtype AbbeyTowerSpiresForbidden = AbbeyTowerSpiresForbidden LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

abbeyTowerSpiresForbidden :: LocationCard AbbeyTowerSpiresForbidden
abbeyTowerSpiresForbidden = location AbbeyTowerSpiresForbidden Cards.abbeyTowerSpiresForbidden 2 (PerPlayer 3) Star [T]

instance HasAbilities AbbeyTowerSpiresForbidden where
  getAbilities (AbbeyTowerSpiresForbidden attrs) =
    getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage AbbeyTowerSpiresForbidden where
  runMessage msg (AbbeyTowerSpiresForbidden attrs) =
    AbbeyTowerSpiresForbidden <$> runMessage msg attrs
