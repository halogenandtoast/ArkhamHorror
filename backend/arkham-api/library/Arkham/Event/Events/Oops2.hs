module Arkham.Event.Events.Oops2 (oops2, Oops2 (..)) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Helpers.Window (attackedEnemy)
import Arkham.Matcher
import Arkham.Modifier

newtype Oops2 = Oops2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

oops2 :: EventCard Oops2
oops2 = event Oops2 Cards.oops2

instance RunMessage Oops2 where
  runMessage msg e@(Oops2 attrs) = runQueueT $ case msg of
    InvestigatorPlayEvent iid eid _ (attackedEnemy -> enemy) _ | eid == toId attrs -> do
      enemies <- filter (/= enemy) <$> select (enemyAtLocationWith iid)
      withSkillTest \sid -> do
        skillTestModifier sid (toSource attrs) iid DoesNotDamageOtherInvestigator
        case enemies of
          [] -> error "event should not have been playable"
          xs -> chooseTargetM iid xs \x -> push $ InvestigatorDamageEnemy iid x (toSource enemy)
      pure e
    _ -> Oops2 <$> liftRunMessage msg attrs
