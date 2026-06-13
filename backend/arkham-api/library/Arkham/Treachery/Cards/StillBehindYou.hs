module Arkham.Treachery.Cards.StillBehindYou (stillBehindYou) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype StillBehindYou = StillBehindYou TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stillBehindYou :: TreacheryCard StillBehindYou
stillBehindYou = treachery StillBehindYou Cards.stillBehindYou

-- TODO: abilities
instance RunMessage StillBehindYou where
  runMessage msg (StillBehindYou attrs) = runQueueT $ StillBehindYou <$> liftRunMessage msg attrs
