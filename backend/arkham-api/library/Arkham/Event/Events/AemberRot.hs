module Arkham.Event.Events.AemberRot (aemberRot) where

import Arkham.Ability
import Arkham.Capability
import Arkham.Enemy.Types (Field (EnemyHealthActual))
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted hiding (EnemyDefeated)
import Arkham.Helpers.Calculation
import Arkham.Matcher
import Arkham.Projection

newtype AemberRot = AemberRot EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aemberRot :: EventCard AemberRot
aemberRot = event AemberRot Cards.aemberRot

instance HasAbilities AemberRot where
  getAbilities (AemberRot a) =
    [ restricted a 1 (ControlsThis <> can.gain.resources You)
        $ forced
        $ EnemyDefeated #when Anyone ByAny
        $ EnemyWithAttachedEvent (be a)
        <> EnemyWithHealth
    , restricted a 2 ControlsThis $ forced $ EnemyLeavesPlay #when $ EnemyWithAttachedEvent (be a)
    ]

instance RunMessage AemberRot where
  runMessage msg e@(AemberRot attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      for_ attrs.attachedTo.enemy \eid -> do
        field EnemyHealthActual eid >>= traverse_ \health -> do
          gainResources iid (attrs.ability 1) . min 5 =<< calculate health
      pure e
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      placeInBonded iid attrs
      pure e
    _ -> AemberRot <$> liftRunMessage msg attrs
