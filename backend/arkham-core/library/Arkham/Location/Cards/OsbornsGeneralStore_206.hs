module Arkham.Location.Cards.OsbornsGeneralStore_206 (
  osbornsGeneralStore_206,
  OsbornsGeneralStore_206 (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (osbornsGeneralStore_206)
import Arkham.Location.Runner

newtype OsbornsGeneralStore_206 = OsbornsGeneralStore_206 LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

osbornsGeneralStore_206 :: LocationCard OsbornsGeneralStore_206
osbornsGeneralStore_206 =
  location OsbornsGeneralStore_206 Cards.osbornsGeneralStore_206 2 (PerPlayer 1)

instance HasModifiersFor OsbornsGeneralStore_206 where
  getModifiersFor (InvestigatorTarget iid) (OsbornsGeneralStore_206 attrs) =
    pure
      $ toModifiers
        attrs
        [CannotGainResources | iid `member` locationInvestigators attrs]
  getModifiersFor _ _ = pure []

instance HasAbilities OsbornsGeneralStore_206 where
  getAbilities = withDrawCardUnderneathAction

instance RunMessage OsbornsGeneralStore_206 where
  runMessage msg (OsbornsGeneralStore_206 attrs) =
    OsbornsGeneralStore_206 <$> runMessage msg attrs
