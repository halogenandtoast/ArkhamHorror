module Arkham.Treachery.Cards.GlimpseTheUnspeakable (glimpseTheUnspeakable) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype GlimpseTheUnspeakable = GlimpseTheUnspeakable TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

glimpseTheUnspeakable :: TreacheryCard GlimpseTheUnspeakable
glimpseTheUnspeakable = treachery GlimpseTheUnspeakable Cards.glimpseTheUnspeakable

instance RunMessage GlimpseTheUnspeakable where
  runMessage msg t@(GlimpseTheUnspeakable attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> GlimpseTheUnspeakable <$> liftRunMessage msg attrs
