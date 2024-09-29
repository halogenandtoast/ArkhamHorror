module Arkham.Treachery.Cards.DraggedUnderDevilReef
  ( draggedUnderDevilReef
  , DraggedUnderDevilReef(..)
  )
where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype DraggedUnderDevilReef = DraggedUnderDevilReef TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

draggedUnderDevilReef :: TreacheryCard DraggedUnderDevilReef
draggedUnderDevilReef = treachery DraggedUnderDevilReef Cards.draggedUnderDevilReef

instance RunMessage DraggedUnderDevilReef where
  runMessage msg t@(DraggedUnderDevilReef attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> DraggedUnderDevilReef <$> liftRunMessage msg attrs
