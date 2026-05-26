module Arkham.Treachery.Cards.EndlessNight (endlessNight) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype EndlessNight = EndlessNight TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

endlessNight :: TreacheryCard EndlessNight
endlessNight = treachery EndlessNight Cards.endlessNight

instance RunMessage EndlessNight where
  runMessage msg t@(EndlessNight attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> do
      pure t
    _ -> EndlessNight <$> liftRunMessage msg attrs
