module Arkham.Event.Cards.CoupDeGrace (coupDeGrace, CoupDeGrace (..)) where

import Arkham.DamageEffect
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
      enemies <- select $ enemyAtLocationWith iid <> EnemyCanBeDamagedBySource (toSource attrs)
      chooseOrRunOne
        iid
        [targetLabel enemy [EnemyDamage enemy $ nonAttack attrs 1] | enemy <- enemies]

      pushWhenM (iid <=~> TurnInvestigator) $ ChooseEndTurn iid
      pure e
    EnemyDefeated _ _ (isSource attrs -> True) _ -> do
      drawCardsIfCan attrs.controller attrs 1
      pure e
    _ -> CoupDeGrace <$> lift (runMessage msg attrs)
