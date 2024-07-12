module Arkham.Event.Cards.Bolas (bolas, Bolas (..)) where

import Arkham.Ability
import Arkham.Aspect
import Arkham.Evade
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Helpers.SkillTest.Target
import Arkham.Matcher
import Arkham.Placement

newtype Bolas = Bolas EventAttrs
  deriving anyclass (IsEvent)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bolas :: EventCard Bolas
bolas = event Bolas Cards.bolas

instance HasModifiersFor Bolas where
  getModifiersFor target (Bolas e) = modified e [EnemyEvade (-1) | e.attachedTo == Just target]

instance HasAbilities Bolas where
  getAbilities (Bolas e) = case e.attachedTo of
    Just (EnemyTarget eid) ->
      [ restrictedAbility e 1 ControlsThis
          $ forced (EnemyEnters #after Anywhere $ EnemyWithId eid)
      ]
    _ -> []

instance RunMessage Bolas where
  runMessage msg e@(Bolas attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      pushAllM $ leftOr <$> aspect iid attrs (#combat `InsteadOf` #agility) (mkChooseEvade iid attrs)
      pure e
    PassedThisSkillTest iid (isSource attrs -> True) -> do
      getSkillTestTarget >>= traverse_ \case
        EnemyTarget eid -> do
          whenM (eid <=~> NonEliteEnemy) $ push $ PlaceEvent iid attrs.id (AttachedToEnemy eid)
        _ -> pure ()
      pure e
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      for_ attrs.attachedTo $ push . Exhaust
      pure e
    _ -> Bolas <$> liftRunMessage msg attrs
