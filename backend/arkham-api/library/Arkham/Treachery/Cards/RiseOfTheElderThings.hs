module Arkham.Treachery.Cards.RiseOfTheElderThings (riseOfTheElderThings) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype RiseOfTheElderThings = RiseOfTheElderThings TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

riseOfTheElderThings :: TreacheryCard RiseOfTheElderThings
riseOfTheElderThings = treachery RiseOfTheElderThings Cards.riseOfTheElderThings

instance RunMessage RiseOfTheElderThings where
  runMessage msg t@(RiseOfTheElderThings attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> RiseOfTheElderThings <$> liftRunMessage msg attrs
