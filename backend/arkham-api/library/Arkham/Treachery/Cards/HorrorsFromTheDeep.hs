module Arkham.Treachery.Cards.HorrorsFromTheDeep
  ( horrorsFromTheDeep
  , HorrorsFromTheDeep(..)
  )
where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype HorrorsFromTheDeep = HorrorsFromTheDeep TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

horrorsFromTheDeep :: TreacheryCard HorrorsFromTheDeep
horrorsFromTheDeep = treachery HorrorsFromTheDeep Cards.horrorsFromTheDeep

instance RunMessage HorrorsFromTheDeep where
  runMessage msg t@(HorrorsFromTheDeep attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> HorrorsFromTheDeep <$> liftRunMessage msg attrs
