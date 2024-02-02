module Arkham.Treachery.Cards.EagerForDeathUnionAndDisillusion (
  EagerForDeathUnionAndDisillusion (..),
  eagerForDeathUnionAndDisillusion,
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Cards.EagerForDeath
import Arkham.Treachery.Runner

newtype EagerForDeathUnionAndDisillusion = EagerForDeathUnionAndDisillusion EagerForDeath
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData, IsTreachery, HasModifiersFor, HasAbilities)

eagerForDeathUnionAndDisillusion :: TreacheryCard EagerForDeathUnionAndDisillusion
eagerForDeathUnionAndDisillusion =
  treachery (EagerForDeathUnionAndDisillusion . EagerForDeath) Cards.eagerForDeathUnionAndDisillusion

instance RunMessage EagerForDeathUnionAndDisillusion where
  runMessage msg (EagerForDeathUnionAndDisillusion inner) =
    EagerForDeathUnionAndDisillusion <$> runMessage msg inner
