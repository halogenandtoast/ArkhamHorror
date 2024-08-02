module Arkham.Event.Cards.ElaborateDistraction (
  elaborateDistraction,
  ElaborateDistraction (..),
)
where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype ElaborateDistraction = ElaborateDistraction EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

elaborateDistraction :: EventCard ElaborateDistraction
elaborateDistraction = event ElaborateDistraction Cards.elaborateDistraction

instance RunMessage ElaborateDistraction where
  runMessage msg e@(ElaborateDistraction attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      handleOneAtATimeSelect iid attrs
        $ EnemyAt (oneOf [locationWithInvestigator iid, ConnectedFrom $ locationWithInvestigator iid])
        <> oneOf
          [NonEliteEnemy <> EnemyCanBeEvadedBy (toSource attrs), EnemyCanBeDamagedBySource (toSource attrs)]
      pure e
    HandleTargetChoice iid (isSource attrs -> True) (EnemyTarget eid) -> do
      canBeEvaded <- eid <=~> (NonEliteEnemy <> EnemyCanBeEvadedBy (toSource attrs))
      canBeDamaged <- eid <=~> EnemyCanBeDamagedBySource (toSource attrs)
      chooseOrRunOneM iid do
        when canBeEvaded
          $ labeled "Automatically evade that enemy if it is not elite"
          $ automaticallyEvadeEnemy iid eid
        when canBeDamaged $ labeled "Deal 1 damage to that enemy" $ nonAttackEnemyDamage attrs 1 eid
      pure e
    _ -> ElaborateDistraction <$> liftRunMessage msg attrs
