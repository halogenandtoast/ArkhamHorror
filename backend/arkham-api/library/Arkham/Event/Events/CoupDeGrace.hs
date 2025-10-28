module Arkham.Event.Events.CoupDeGrace (coupDeGrace) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Matcher hiding (EnemyDefeated, NonAttackDamageEffect)

newtype CoupDeGrace = CoupDeGrace EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

coupDeGrace :: EventCard CoupDeGrace
coupDeGrace = event CoupDeGrace Cards.coupDeGrace

instance RunMessage CoupDeGrace where
  runMessage msg e@(CoupDeGrace attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      chooseDamageEnemy iid attrs (locationWithInvestigator iid) AnyEnemy 1
      pushWhenM (iid <=~> TurnInvestigator) $ ChooseEndTurn iid
      pure e
    EnemyDefeated _ _ (isSource attrs -> True) _ -> do
      drawCardsIfCan attrs.controller attrs 1
      pure e
    _ -> CoupDeGrace <$> liftRunMessage msg attrs
