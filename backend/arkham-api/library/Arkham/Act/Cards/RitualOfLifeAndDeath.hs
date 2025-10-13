module Arkham.Act.Cards.RitualOfLifeAndDeath (ritualOfLifeAndDeath) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype RitualOfLifeAndDeath = RitualOfLifeAndDeath ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

ritualOfLifeAndDeath :: ActCard RitualOfLifeAndDeath
ritualOfLifeAndDeath = act (2, A) RitualOfLifeAndDeath Cards.ritualOfLifeAndDeath Nothing

instance RunMessage RitualOfLifeAndDeath where
  runMessage msg a@(RitualOfLifeAndDeath attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> RitualOfLifeAndDeath <$> liftRunMessage msg attrs
