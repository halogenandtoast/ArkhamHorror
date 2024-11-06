module Arkham.Treachery.Cards.SecretGathering
  ( secretGathering
  , SecretGathering(..)
  )
where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype SecretGathering = SecretGathering TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

secretGathering :: TreacheryCard SecretGathering
secretGathering = treachery SecretGathering Cards.secretGathering

instance RunMessage SecretGathering where
  runMessage msg t@(SecretGathering attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> SecretGathering <$> liftRunMessage msg attrs
