module Arkham.Treachery.Cards.ChildrenOfBlood.AgentsOfZburamoarte.UnnaturalStrength (unnaturalStrength) where

import Arkham.Treachery.CardDefs.ChildrenOfBlood.AgentsOfZburamoarte qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype UnnaturalStrength = UnnaturalStrength TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unnaturalStrength :: TreacheryCard UnnaturalStrength
unnaturalStrength = treachery UnnaturalStrength Cards.unnaturalStrength

instance RunMessage UnnaturalStrength where
  runMessage msg t@(UnnaturalStrength attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> UnnaturalStrength <$> liftRunMessage msg attrs
