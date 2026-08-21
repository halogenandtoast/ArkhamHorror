module Arkham.Treachery.Cards.ChildrenOfBlood.Afflicted.TorturousTransformation (torturousTransformation) where

import Arkham.Treachery.CardDefs.ChildrenOfBlood.Afflicted qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype TorturousTransformation = TorturousTransformation TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

torturousTransformation :: TreacheryCard TorturousTransformation
torturousTransformation = treachery TorturousTransformation Cards.torturousTransformation

instance RunMessage TorturousTransformation where
  runMessage msg t@(TorturousTransformation attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> TorturousTransformation <$> liftRunMessage msg attrs
