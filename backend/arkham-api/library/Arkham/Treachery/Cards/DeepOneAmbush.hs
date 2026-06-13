module Arkham.Treachery.Cards.DeepOneAmbush (deepOneAmbush) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype DeepOneAmbush = DeepOneAmbush TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

deepOneAmbush :: TreacheryCard DeepOneAmbush
deepOneAmbush = treachery DeepOneAmbush Cards.deepOneAmbush

-- TODO: abilities
instance RunMessage DeepOneAmbush where
  runMessage msg (DeepOneAmbush attrs) = runQueueT $ DeepOneAmbush <$> liftRunMessage msg attrs
