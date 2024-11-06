module Arkham.Treachery.Cards.StoneBarrier
  ( stoneBarrier
  , StoneBarrier(..)
  )
where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype StoneBarrier = StoneBarrier TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stoneBarrier :: TreacheryCard StoneBarrier
stoneBarrier = treachery StoneBarrier Cards.stoneBarrier

instance RunMessage StoneBarrier where
  runMessage msg t@(StoneBarrier attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> StoneBarrier <$> liftRunMessage msg attrs
