module Arkham.Event.Cards.Lifeline1 (lifeline1, Lifeline1 (..)) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.History
import Arkham.SkillTestResult
import Arkham.Strategy
import Arkham.Window

newtype Lifeline1 = Lifeline1 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lifeline1 :: EventCard Lifeline1
lifeline1 = eventWith Lifeline1 Cards.lifeline1 $ afterPlayL .~ ExileThis

instance RunMessage Lifeline1 where
  runMessage msg e@(Lifeline1 attrs) = runQueueT $ case msg of
    PlayThisEvent _iid (is attrs -> True) -> do
      let miid = listToMaybe [iid | (windowType -> WouldEndTurn iid) <- attrs.windows]
      for_ miid \iid -> do
        x <- getHistoryField TurnHistory iid HistorySkillTestsPerformed
        let failed = count (isFailedResult . snd) x
        cancelEndTurn iid
        push $ GainActions iid (toSource attrs) failed
        push $ PlayerWindow iid [] False
      pure e
    _ -> Lifeline1 <$> liftRunMessage msg attrs
