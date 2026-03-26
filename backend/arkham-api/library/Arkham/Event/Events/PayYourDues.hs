module Arkham.Event.Events.PayYourDues (payYourDues) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.GameValue
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher
import Arkham.Modifier

newtype PayYourDues = PayYourDues EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

payYourDues :: EventCard PayYourDues
payYourDues = event PayYourDues Cards.payYourDues

instance RunMessage PayYourDues where
  runMessage msg e@(PayYourDues attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      selectOneToHandle iid attrs
        $ NonEliteEnemy
        <> enemyAtLocationWith iid
        <> EnemyWithRemainingHealth (EqualTo $ Static attrs.payment.resources)
      pure e
    HandleTargetChoice iid (isSource attrs -> True) (EnemyTarget eid) -> do
      disengageEnemy iid eid
      roundModifier attrs eid (AddKeyword Keyword.Aloof)
      discoverAtYourLocation NotInvestigate iid attrs 1
      pure e
    _ -> PayYourDues <$> liftRunMessage msg attrs
