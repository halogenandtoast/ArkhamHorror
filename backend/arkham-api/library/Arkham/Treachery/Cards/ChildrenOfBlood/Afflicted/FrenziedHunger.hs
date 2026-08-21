module Arkham.Treachery.Cards.ChildrenOfBlood.Afflicted.FrenziedHunger (frenziedHunger) where

import Arkham.Treachery.CardDefs.ChildrenOfBlood.Afflicted qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype FrenziedHunger = FrenziedHunger TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

frenziedHunger :: TreacheryCard FrenziedHunger
frenziedHunger = treachery FrenziedHunger Cards.frenziedHunger

instance RunMessage FrenziedHunger where
  runMessage msg t@(FrenziedHunger attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> FrenziedHunger <$> liftRunMessage msg attrs
