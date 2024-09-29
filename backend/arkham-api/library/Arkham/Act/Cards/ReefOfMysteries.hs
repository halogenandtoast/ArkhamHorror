module Arkham.Act.Cards.ReefOfMysteries
  ( ReefOfMysteries(..)
  , reefOfMysteries
  ) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype ReefOfMysteries = ReefOfMysteries ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

reefOfMysteries :: ActCard ReefOfMysteries
reefOfMysteries = act (1, A) ReefOfMysteries Cards.reefOfMysteries Nothing

instance RunMessage ReefOfMysteries where
  runMessage msg a@(ReefOfMysteries attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> ReefOfMysteries <$> liftRunMessage msg attrs
