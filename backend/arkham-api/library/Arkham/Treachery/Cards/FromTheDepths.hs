module Arkham.Treachery.Cards.FromTheDepths
  ( fromTheDepths
  , FromTheDepths(..)
  )
where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype FromTheDepths = FromTheDepths TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fromTheDepths :: TreacheryCard FromTheDepths
fromTheDepths = treachery FromTheDepths Cards.fromTheDepths

instance RunMessage FromTheDepths where
  runMessage msg t@(FromTheDepths attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> FromTheDepths <$> liftRunMessage msg attrs
