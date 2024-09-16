module Arkham.Treachery.Cards.RisingTides (risingTides, RisingTides (..)) where

import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype RisingTides = RisingTides TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

risingTides :: TreacheryCard RisingTides
risingTides = treachery RisingTides Cards.risingTides

instance RunMessage RisingTides where
  runMessage msg t@(RisingTides attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      locations <- select $ NearestLocationTo iid CanHaveFloodLevelIncreased
      if null locations
        then gainSurge attrs
        else chooseTargetM iid locations $ push . IncreaseFloodLevel
      pure t
    _ -> RisingTides <$> liftRunMessage msg attrs
