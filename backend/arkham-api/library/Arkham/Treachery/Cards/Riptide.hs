module Arkham.Treachery.Cards.Riptide
  ( riptide
  , Riptide(..)
  )
where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Riptide = Riptide TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

riptide :: TreacheryCard Riptide
riptide = treachery Riptide Cards.riptide

instance RunMessage Riptide where
  runMessage msg t@(Riptide attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> Riptide <$> liftRunMessage msg attrs
