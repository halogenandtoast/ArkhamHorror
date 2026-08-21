module Arkham.Treachery.Cards.ChildrenOfBlood.Afflicted.TorturousTransformation (torturousTransformation) where

import Arkham.Helpers.Message.Discard.Lifted
import Arkham.Matcher
import Arkham.Treachery.CardDefs.ChildrenOfBlood.Afflicted qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype TorturousTransformation = TorturousTransformation TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

torturousTransformation :: TreacheryCard TorturousTransformation
torturousTransformation = treachery TorturousTransformation Cards.torturousTransformation

instance RunMessage TorturousTransformation where
  runMessage msg t@(TorturousTransformation attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      n <- selectCount $ SealedOnInvestigator (InvestigatorWithId iid) #blood
      if n == 0
        then loseResources iid attrs 2
        else do
          assignDamageAndHorror iid attrs 1 1
          when (n >= 3) $ randomDiscard iid attrs
      pure t
    _ -> TorturousTransformation <$> liftRunMessage msg attrs
