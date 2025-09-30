module Arkham.Treachery.Cards.EagerForDeathUnionAndDisillusion ( eagerForDeathUnionAndDisillusion,) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Cards.EagerForDeath
import Arkham.Treachery.Import.Lifted

newtype EagerForDeathUnionAndDisillusion = EagerForDeathUnionAndDisillusion EagerForDeath
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, IsTreachery, HasModifiersFor, HasAbilities)

eagerForDeathUnionAndDisillusion :: TreacheryCard EagerForDeathUnionAndDisillusion
eagerForDeathUnionAndDisillusion =
  treachery (EagerForDeathUnionAndDisillusion . EagerForDeath) Cards.eagerForDeathUnionAndDisillusion

instance RunMessage EagerForDeathUnionAndDisillusion where
  runMessage msg (EagerForDeathUnionAndDisillusion inner) =
    EagerForDeathUnionAndDisillusion <$> runMessage msg inner
