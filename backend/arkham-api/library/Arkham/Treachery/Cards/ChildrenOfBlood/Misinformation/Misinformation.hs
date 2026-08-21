module Arkham.Treachery.Cards.ChildrenOfBlood.Misinformation.Misinformation (misinformation) where

import Arkham.Treachery.CardDefs.ChildrenOfBlood.Misinformation qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Misinformation = Misinformation TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

misinformation :: TreacheryCard Misinformation
misinformation = treachery Misinformation Cards.misinformation

instance RunMessage Misinformation where
  runMessage msg t@(Misinformation attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> Misinformation <$> liftRunMessage msg attrs
