module Arkham.Event.Events.ReadyForAnything (readyForAnything) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.History
import Arkham.Helpers.Investigator (canHaveHorrorHealed)
import {-# SOURCE #-} Arkham.GameEnv (getHistoryField)

newtype ReadyForAnything = ReadyForAnything EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

readyForAnything :: EventCard ReadyForAnything
readyForAnything = event ReadyForAnything Cards.readyForAnything

instance RunMessage ReadyForAnything where
  runMessage msg e@(ReadyForAnything attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      drawnEnemies <- getHistoryField #round iid HistoryEnemiesDrawn
      drawCards iid attrs $ if null drawnEnemies then 1 else 2
      canHeal <- canHaveHorrorHealed attrs iid
      when canHeal $ healHorror iid attrs 1
      pure e
    _ -> ReadyForAnything <$> liftRunMessage msg attrs
