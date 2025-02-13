module Arkham.Treachery.Cards.CastAdrift (castAdrift) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted
import Arkham.Investigator.Types (Field(..))
import Arkham.Projection

newtype CastAdrift = CastAdrift TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

castAdrift :: TreacheryCard CastAdrift
castAdrift = treachery CastAdrift Cards.castAdrift

instance RunMessage CastAdrift where
  runMessage msg t@(CastAdrift attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      under <- field InvestigatorCardsUnderneath iid
      if null under
        then shuffleIntoDeck iid attrs
        else addToDiscard iid under
      pure t
    _ -> CastAdrift <$> liftRunMessage msg attrs
