module Arkham.Treachery.Cards.DeepShadows (deepShadows) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype DeepShadows = DeepShadows TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

deepShadows :: TreacheryCard DeepShadows
deepShadows = treachery DeepShadows Cards.deepShadows

instance RunMessage DeepShadows where
  runMessage msg t@(DeepShadows attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> DeepShadows <$> liftRunMessage msg attrs
