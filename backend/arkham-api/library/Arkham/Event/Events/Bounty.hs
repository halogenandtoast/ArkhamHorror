module Arkham.Event.Events.Bounty (bounty) where

import Arkham.Capability
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Enemy (getDefeatedEnemyHealth)
import Arkham.Helpers.Window (defeatedEnemy)
import Arkham.Matcher

newtype Bounty = Bounty EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bounty :: EventCard Bounty
bounty = event Bounty Cards.bounty

instance RunMessage Bounty where
  runMessage msg e@(Bounty attrs) = runQueueT $ case msg of
    PlayThisEvent _iid (is attrs -> True) -> do
      let eid = defeatedEnemy attrs.windows
      x <- maybe 0 (min 6) <$> getDefeatedEnemyHealth eid
      doStep x msg
      pure e
    DoStep x msg'@(PlayThisEvent iid (is attrs -> True)) | x > 0 -> do
      iids <- select $ affectsOthers $ colocatedWith iid <> can.gain.resources
      chooseOrRunOneM iid $ for_ iids \iid' -> resourceLabeled iid' $ gainResources iid' attrs 1
      doStep (x - 1) msg'
      pure e
    _ -> Bounty <$> liftRunMessage msg attrs
