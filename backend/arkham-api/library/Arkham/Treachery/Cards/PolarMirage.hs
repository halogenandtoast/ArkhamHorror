module Arkham.Treachery.Cards.PolarMirage (polarMirage, PolarMirage (..)) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype PolarMirage = PolarMirage TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

polarMirage :: TreacheryCard PolarMirage
polarMirage = treachery PolarMirage Cards.polarMirage

instance RunMessage PolarMirage where
  runMessage msg t@(PolarMirage attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> PolarMirage <$> liftRunMessage msg attrs
