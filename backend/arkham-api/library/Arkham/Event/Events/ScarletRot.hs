module Arkham.Event.Events.ScarletRot (scarletRot) where

import Arkham.Ability
import Arkham.Card
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted hiding (EnemyDefeated)
import Arkham.Matcher

newtype ScarletRot = ScarletRot EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

scarletRot :: EventCard ScarletRot
scarletRot = event ScarletRot Cards.scarletRot

instance HasAbilities ScarletRot where
  getAbilities (ScarletRot a) =
    [ controlled a 1 (exists (EnemyWithAttachedEvent (be a) <> EnemyCanBeDamagedBySource (a.ability 1)))
        $ forced
        $ RoundEnds #when
    , restricted a 2 ControlsThis $ forced $ EnemyLeavesPlay #when $ EnemyWithAttachedEvent (be a)
    ]

instance RunMessage ScarletRot where
  runMessage msg e@(ScarletRot attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      for_ attrs.attachedTo \case
        EnemyTarget eid -> nonAttackEnemyDamage (Just iid) (attrs.ability 1) 1 eid
        _ -> pure ()
      pure e
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      push $ PlaceInBonded iid (toCard attrs)
      pure e
    _ -> ScarletRot <$> liftRunMessage msg attrs
