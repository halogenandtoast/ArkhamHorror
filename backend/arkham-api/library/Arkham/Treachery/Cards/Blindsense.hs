module Arkham.Treachery.Cards.Blindsense
  ( blindsense
  , Blindsense(..)
  )
where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Blindsense = Blindsense TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

blindsense :: TreacheryCard Blindsense
blindsense = treachery Blindsense Cards.blindsense

instance RunMessage Blindsense where
  runMessage msg t@(Blindsense attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> Blindsense <$> liftRunMessage msg attrs
