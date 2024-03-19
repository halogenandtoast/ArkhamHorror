module Arkham.Treachery.Cards.WhisperingChaosNorth
  ( whisperingChaosNorth
  , WhisperingChaosNorth(..)
  )
where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype WhisperingChaosNorth = WhisperingChaosNorth TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

whisperingChaosNorth :: TreacheryCard WhisperingChaosNorth
whisperingChaosNorth = treachery WhisperingChaosNorth Cards.whisperingChaosNorth

instance RunMessage WhisperingChaosNorth where
  runMessage msg t@(WhisperingChaosNorth attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> WhisperingChaosNorth <$> lift (runMessage msg attrs)
