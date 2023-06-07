module Arkham.Location.Cards.SanctumDoorwayCeremonyRoom
  ( sanctumDoorwayCeremonyRoom
  , SanctumDoorwayCeremonyRoom(..)
  )
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype SanctumDoorwayCeremonyRoom = SanctumDoorwayCeremonyRoom LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sanctumDoorwayCeremonyRoom :: LocationCard SanctumDoorwayCeremonyRoom
sanctumDoorwayCeremonyRoom = location SanctumDoorwayCeremonyRoom Cards.sanctumDoorwayCeremonyRoom 3 (PerPlayer 2)

instance HasAbilities SanctumDoorwayCeremonyRoom where
  getAbilities (SanctumDoorwayCeremonyRoom attrs) =
    getAbilities attrs
    -- withRevealedAbilities attrs []

instance RunMessage SanctumDoorwayCeremonyRoom where
  runMessage msg (SanctumDoorwayCeremonyRoom attrs) =
    SanctumDoorwayCeremonyRoom <$> runMessage msg attrs
