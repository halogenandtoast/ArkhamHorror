module Arkham.Treachery.Cards.TheBellTolls (theBellTolls) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype TheBellTolls = TheBellTolls TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theBellTolls :: TreacheryCard TheBellTolls
theBellTolls = treachery TheBellTolls Cards.theBellTolls

instance RunMessage TheBellTolls where
  runMessage msg t@(TheBellTolls attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      kill attrs iid
      pure t
    _ -> TheBellTolls <$> liftRunMessage msg attrs
