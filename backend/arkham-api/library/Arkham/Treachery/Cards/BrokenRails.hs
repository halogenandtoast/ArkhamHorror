module Arkham.Treachery.Cards.BrokenRails (brokenRails) where

import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted hiding (InvestigatorDamage)

newtype BrokenRails = BrokenRails TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

brokenRails :: TreacheryCard BrokenRails
brokenRails = treachery BrokenRails Cards.brokenRails

instance RunMessage BrokenRails where
  runMessage msg t@(BrokenRails attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      investigators <- select $ colocatedWith iid
      for_ investigators \iid' -> loseActions iid' attrs 1

      investigatorsWhoMustDiscard <- filterByField InvestigatorDamage (>= 4) investigators
      for_ investigatorsWhoMustDiscard (`chooseAndDiscardAsset` attrs)
      pure t
    _ -> BrokenRails <$> liftRunMessage msg attrs
