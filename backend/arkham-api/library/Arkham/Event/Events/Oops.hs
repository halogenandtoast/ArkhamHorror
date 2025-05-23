module Arkham.Event.Events.Oops (oops) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Window
import Arkham.Matcher

newtype Oops = Oops EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

oops :: EventCard Oops
oops = event Oops Cards.oops

instance RunMessage Oops where
  runMessage msg e@(Oops attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      let enemy = attackedEnemy attrs.windows
      select (enemyAtLocationWith iid <> not_ (be enemy)) >>= \case
        [] -> error "event should not have been playable"
        [x] -> push $ InvestigatorDamageEnemy iid x (toSource enemy)
        xs -> chooseTargetM iid xs \x -> push $ InvestigatorDamageEnemy iid x (toSource enemy)
      pure e
    _ -> Oops <$> liftRunMessage msg attrs
