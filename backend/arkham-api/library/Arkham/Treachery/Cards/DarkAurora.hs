module Arkham.Treachery.Cards.DarkAurora
  ( darkAurora
  , DarkAurora(..)
  )
where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype DarkAurora = DarkAurora TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

darkAurora :: TreacheryCard DarkAurora
darkAurora = treachery DarkAurora Cards.darkAurora

instance RunMessage DarkAurora where
  runMessage msg t@(DarkAurora attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> DarkAurora <$> liftRunMessage msg attrs
