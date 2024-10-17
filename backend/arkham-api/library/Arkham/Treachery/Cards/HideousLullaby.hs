module Arkham.Treachery.Cards.HideousLullaby
  ( hideousLullaby
  , HideousLullaby(..)
  )
where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype HideousLullaby = HideousLullaby TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hideousLullaby :: TreacheryCard HideousLullaby
hideousLullaby = treachery HideousLullaby Cards.hideousLullaby

instance RunMessage HideousLullaby where
  runMessage msg t@(HideousLullaby attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> HideousLullaby <$> liftRunMessage msg attrs
