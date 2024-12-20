module Arkham.Treachery.Cards.BlasphemousVisions (blasphemousVisions) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype BlasphemousVisions = BlasphemousVisions TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

blasphemousVisions :: TreacheryCard BlasphemousVisions
blasphemousVisions = treachery BlasphemousVisions Cards.blasphemousVisions

instance RunMessage BlasphemousVisions where
  runMessage msg t@(BlasphemousVisions attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> BlasphemousVisions <$> liftRunMessage msg attrs
