module Arkham.Event.Cards.BuryThemDeep (buryThemDeep, BuryThemDeep (..)) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Window (defeatedEnemy)

newtype BuryThemDeep = BuryThemDeep EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

buryThemDeep :: EventCard BuryThemDeep
buryThemDeep = event BuryThemDeep Cards.buryThemDeep

instance RunMessage BuryThemDeep where
  runMessage msg e@(BuryThemDeep attrs) = runQueueT $ case msg of
    InvestigatorPlayEvent _ (is attrs -> True) _ (defeatedEnemy -> enemyId) _ -> do
      addToVictory attrs
      insteadOfMatching (\case Discard _ _ t -> enemyId `is` t; _ -> False) (addToVictory enemyId)
      pure e
    _ -> BuryThemDeep <$> liftRunMessage msg attrs
