module Arkham.Treachery.Cards.RisingTides
  ( risingTides
  , RisingTides(..)
  )
where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype RisingTides = RisingTides TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

risingTides :: TreacheryCard RisingTides
risingTides = treachery RisingTides Cards.risingTides

instance RunMessage RisingTides where
  runMessage msg t@(RisingTides attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> RisingTides <$> liftRunMessage msg attrs
