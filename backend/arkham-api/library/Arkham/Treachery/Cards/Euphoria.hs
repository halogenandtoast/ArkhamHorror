module Arkham.Treachery.Cards.Euphoria (euphoria) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Euphoria = Euphoria TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

euphoria :: TreacheryCard Euphoria
euphoria = treachery Euphoria Cards.euphoria

instance RunMessage Euphoria where
  runMessage msg (Euphoria attrs) =
    runQueueT $ Euphoria <$> liftRunMessage msg attrs
