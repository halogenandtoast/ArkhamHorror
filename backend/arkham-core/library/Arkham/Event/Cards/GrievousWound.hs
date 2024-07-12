module Arkham.Event.Cards.GrievousWound (grievousWound, GrievousWound (..)) where

import Arkham.Ability
import Arkham.DamageEffect
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Window (attackedEnemy)
import Arkham.Matcher
import Arkham.Placement

newtype GrievousWound = GrievousWound EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

grievousWound :: EventCard GrievousWound
grievousWound = event GrievousWound Cards.grievousWound

instance HasAbilities GrievousWound where
  getAbilities (GrievousWound a) = case a.attachedTo of
    Just (EnemyTarget eid) ->
      [ restrictedAbility
          a
          1
          (ControlsThis <> exists (EnemyWithId eid <> EnemyCanBeDamagedBySource (a.ability 1)))
          $ forced
          $ RoundEnds #when
      ]
    _ -> []

instance RunMessage GrievousWound where
  runMessage msg e@(GrievousWound attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      let enemy = attackedEnemy attrs.windows
      push $ PlaceEvent iid attrs.id (AttachedToEnemy enemy)
      pure e
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      for_ attrs.attachedTo \case
        EnemyTarget eid -> push $ EnemyDamage eid $ nonAttack (attrs.ability 1) 1
        _ -> pure ()
      pure e
    _ -> GrievousWound <$> liftRunMessage msg attrs
