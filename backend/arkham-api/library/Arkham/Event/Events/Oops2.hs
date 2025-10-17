module Arkham.Event.Events.Oops2 (oops2) where

import Arkham.Campaigns.TheScarletKeys.Concealed.Helpers
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
      enemies <- select (enemyAtLocationWith iid)
      mconcealed <- getConcealed (ForExpose $ toSource iid) iid
      withSkillTest \sid -> do
        skillTestModifier sid (toSource attrs) iid DoesNotDamageOtherInvestigator
        chooseOrRunOneM iid do
          targets enemies \x -> push $ InvestigatorDamageEnemy iid x (toSource enemy)
          for_ mconcealed \concealed -> targeting concealed $ doFlip iid attrs concealed
      pure e
    _ -> Oops2 <$> liftRunMessage msg attrs
