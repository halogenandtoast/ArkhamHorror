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
    PlayThisEvent iid (is attrs -> True) -> do
      let eid = defeatedEnemy attrs.windows
      mhealth <- getDefeatedEnemyHealth eid
      let x = maybe 0 (min 6) mhealth
      when (x > 0) do
        iids <- select $ affectsOthers $ colocatedWith iid <> can.gain.resources
        replicateM_ x do
          chooseOrRunOne
            iid
            [ ResourceLabel iid' [TakeResources iid' 1 (toSource attrs) False]
            | iid' <- iids
            ]
      pure e
    _ -> Bounty <$> liftRunMessage msg attrs
