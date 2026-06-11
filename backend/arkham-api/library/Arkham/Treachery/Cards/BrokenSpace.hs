module Arkham.Treachery.Cards.BrokenSpace (brokenSpace) where

import Arkham.Helpers.Location (getLocationOf)
import Arkham.Investigator.Projection ()
import Arkham.Location.Types (Field (..))
import Arkham.Projection
import Arkham.Trait (Trait (Future, Past, Present))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype BrokenSpace = BrokenSpace TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

brokenSpace :: TreacheryCard BrokenSpace
brokenSpace = treachery BrokenSpace Cards.brokenSpace

instance RunMessage BrokenSpace where
  runMessage msg t@(BrokenSpace attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      getLocationOf iid >>= traverse_ \lid -> do
        traits <- field LocationTraits lid
        when (Past `member` traits) $ loseResources iid attrs 3
        when (Present `member` traits) do
          clues <- iid.clues
          when (clues > 0) $ push $ InvestigatorPlaceCluesOnLocation iid (toSource attrs) (min 2 clues)
        when (Future `member` traits) $ loseActions iid attrs 1
      pure t
    _ -> BrokenSpace <$> liftRunMessage msg attrs
