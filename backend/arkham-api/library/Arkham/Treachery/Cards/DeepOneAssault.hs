module Arkham.Treachery.Cards.DeepOneAssault
  ( deepOneAssault
  , DeepOneAssault(..)
  )
where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype DeepOneAssault = DeepOneAssault TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

deepOneAssault :: TreacheryCard DeepOneAssault
deepOneAssault = treachery DeepOneAssault Cards.deepOneAssault

instance RunMessage DeepOneAssault where
  runMessage msg t@(DeepOneAssault attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> DeepOneAssault <$> liftRunMessage msg attrs
