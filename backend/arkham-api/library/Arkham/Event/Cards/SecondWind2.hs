module Arkham.Event.Cards.SecondWind2 (secondWind2, SecondWind2 (..)) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.History

newtype SecondWind2 = SecondWind2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

secondWind2 :: EventCard SecondWind2
secondWind2 = event SecondWind2 Cards.secondWind2

instance RunMessage SecondWind2 where
  runMessage msg e@(SecondWind2 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      roundHistory <- getHistory RoundHistory iid
      let damageToHeal = if null (historyTreacheriesDrawn roundHistory) then 2 else 3
      push $ HealDamage (InvestigatorTarget iid) (toSource attrs) damageToHeal
      drawCardsIfCan iid attrs 1
      pure e
    _ -> SecondWind2 <$> liftRunMessage msg attrs
