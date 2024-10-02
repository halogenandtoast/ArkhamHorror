module Arkham.Event.Events.SecondWind (secondWind, SecondWind (..)) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.History

newtype SecondWind = SecondWind EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

secondWind :: EventCard SecondWind
secondWind = event SecondWind Cards.secondWind

instance RunMessage SecondWind where
  runMessage msg e@(SecondWind attrs) = runQueueT case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      drawn <- getHistoryField RoundHistory iid HistoryTreacheriesDrawn
      let damageToHeal = if null drawn then 1 else 2
      healDamage iid attrs damageToHeal
      drawCardsIfCan iid attrs 1
      pure e
    _ -> SecondWind <$> liftRunMessage msg attrs
