module Arkham.Event.Events.Bolas (bolas) where

import Arkham.Ability
import Arkham.Aspect hiding (aspect)
import Arkham.Evade
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Helpers.SkillTest.Target
import Arkham.Matcher

newtype Bolas = Bolas EventAttrs
  deriving anyclass IsEvent
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bolas :: EventCard Bolas
bolas = event Bolas Cards.bolas

instance HasModifiersFor Bolas where
  getModifiersFor (Bolas e) = for_ e.attachedTo.enemy \target ->
    modified_ e target [EnemyEvade (-1)]

instance HasAbilities Bolas where
  getAbilities (Bolas e) = case e.attachedTo.enemy of
    Just eid -> [restricted e 1 ControlsThis $ forced $ EnemyEnters #after Anywhere (be eid)]
    _ -> []

instance RunMessage Bolas where
  runMessage msg e@(Bolas attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      sid <- getRandom
      aspect iid attrs (#combat `InsteadOf` #agility) (mkChooseEvade sid iid attrs)
      pure e
    PassedThisSkillTest _iid (isSource attrs -> True) -> do
      whenJustM getSkillTestTarget \case
        EnemyTarget eid -> do
          whenM (eid <=~> NonEliteEnemy) $ place attrs.id (AttachedToEnemy eid)
        _ -> pure ()
      pure e
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      for_ attrs.attachedTo.enemy exhaustThis
      pure e
    _ -> Bolas <$> liftRunMessage msg attrs
