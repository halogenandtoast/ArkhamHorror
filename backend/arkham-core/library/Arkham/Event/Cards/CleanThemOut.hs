module Arkham.Event.Cards.CleanThemOut (cleanThemOut, CleanThemOut (..)) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Fight

newtype CleanThemOut = CleanThemOut EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cleanThemOut :: EventCard CleanThemOut
cleanThemOut = event CleanThemOut Cards.cleanThemOut

instance RunMessage CleanThemOut where
  runMessage msg e@(CleanThemOut attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      gainResourcesIfCan iid attrs 2
      sid <- getRandom
      pushM $ mkChooseFight sid iid attrs
      pure e
    _ -> CleanThemOut <$> liftRunMessage msg attrs
