module Arkham.Location.Cards.LodgeCellarMembersOnly
  ( lodgeCellarMembersOnly
  , LodgeCellarMembersOnly(..)
  )
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype LodgeCellarMembersOnly = LodgeCellarMembersOnly LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lodgeCellarMembersOnly :: LocationCard LodgeCellarMembersOnly
lodgeCellarMembersOnly = location LodgeCellarMembersOnly Cards.lodgeCellarMembersOnly 3 (Static 0)

instance HasAbilities LodgeCellarMembersOnly where
  getAbilities (LodgeCellarMembersOnly attrs) =
    getAbilities attrs
    -- withRevealedAbilities attrs []

instance RunMessage LodgeCellarMembersOnly where
  runMessage msg (LodgeCellarMembersOnly attrs) =
    LodgeCellarMembersOnly <$> runMessage msg attrs
