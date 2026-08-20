module Arkham.Treachery.Cards.TheCircleUndone.UnionAndDisillusion.EagerForDeath (eagerForDeath) where

import Arkham.Treachery.CardDefs.TheCircleUndone.UnionAndDisillusion qualified as Cards
import Arkham.Treachery.Cards.TheDunwichLegacy.Whippoorwills.EagerForDeath qualified as Base
import Arkham.Treachery.Import.Lifted

newtype EagerForDeath = EagerForDeath Base.EagerForDeath
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, IsTreachery, HasModifiersFor, HasAbilities)

eagerForDeath :: TreacheryCard EagerForDeath
eagerForDeath =
  treachery
    (EagerForDeath . Base.EagerForDeath)
    Cards.eagerForDeath

instance RunMessage EagerForDeath where
  runMessage msg (EagerForDeath inner) =
    EagerForDeath <$> runMessage msg inner
