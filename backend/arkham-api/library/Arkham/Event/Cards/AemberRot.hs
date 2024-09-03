module Arkham.Event.Cards.AemberRot (aemberRot, AemberRot (..)) where

import Arkham.Ability
import Arkham.Capability
import Arkham.Card
import Arkham.Enemy.Types (Field (..))
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted hiding (EnemyDefeated)
import Arkham.Helpers.GameValue
import Arkham.Matcher
import Arkham.Projection

newtype AemberRot = AemberRot EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aemberRot :: EventCard AemberRot
aemberRot = event AemberRot Cards.aemberRot

instance HasAbilities AemberRot where
  getAbilities (AemberRot a) =
    [ restrictedAbility a 1 (ControlsThis <> can.gain.resources You)
        $ forced
        $ EnemyDefeated #when Anyone ByAny
        $ EnemyWithAttachedEvent (be a)
        <> EnemyWithHealth
    , restrictedAbility a 2 ControlsThis $ forced $ EnemyLeavesPlay #when $ EnemyWithAttachedEvent (be a)
    ]

instance RunMessage AemberRot where
  runMessage msg e@(AemberRot attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      for_ attrs.attachedTo \case
        EnemyTarget eid -> do
          field EnemyHealthActual eid >>= traverse_ \health -> do
            gainResourcesIfCan iid (attrs.ability 1) . min 5 =<< getPlayerCountValue health
        _ -> pure ()
      pure e
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      push $ PlaceInBonded iid (toCard attrs)
      pure e
    _ -> AemberRot <$> liftRunMessage msg attrs
