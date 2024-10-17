module Arkham.Treachery.Cards.TakenCaptive
  ( takenCaptive
  , TakenCaptive(..)
  )
where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype TakenCaptive = TakenCaptive TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

takenCaptive :: TreacheryCard TakenCaptive
takenCaptive = treachery TakenCaptive Cards.takenCaptive

instance RunMessage TakenCaptive where
  runMessage msg t@(TakenCaptive attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> TakenCaptive <$> liftRunMessage msg attrs
