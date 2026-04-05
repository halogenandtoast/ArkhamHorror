module Arkham.Event.Events.LieInWait (lieInWait) where

import Arkham.Ability
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Fight.Types
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.Window (enteringEnemy)
import Arkham.Matcher
import Arkham.Modifier

newtype LieInWait = LieInWait EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lieInWait :: EventCard LieInWait
lieInWait = event LieInWait Cards.lieInWait

instance HasAbilities LieInWait where
  getAbilities (LieInWait a) = case a.attachedTo.location of
    Just lid ->
      [ withFightCriteriaOverride (CriteriaOverride canFightAtAnyLocation)
          $ controlled_ a 1
          $ triggeredAction #fight (EnemyEnters #after (LocationWithId lid) AnyEnemy) (exhaust a)
      ]
    _ -> []

instance RunMessage LieInWait where
  runMessage msg e@(LieInWait attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      withLocationOf iid $ place attrs . AttachedToLocation
      pure e
    UseCardAbility iid (isSource attrs -> True) 1 (enteringEnemy -> eid) _ -> do
      sid <- getRandom
      let
        using sk = skillLabeled sk do
          chooseOneM iid do
            labeled "Fight without discarding (+1 skill)" do
              skillTestModifier sid (attrs.ability 1) iid (AnySkillValue 1)
            labeled "Discard Lie in Wait to fight with +1 skill and +1 damage" do
              toDiscardBy iid (attrs.ability 1) attrs
              skillTestModifiers sid (attrs.ability 1) iid [AnySkillValue 1, DamageDealt 1]
          chooseFightEnemyEdit sid iid (attrs.ability 1) \cf ->
            cf
              { chooseFightEnemyMatcher = fightOverride (EnemyWithId eid)
              , chooseFightOverride = True
              , chooseFightSkillType = sk
              }
      chooseOneM iid do
        using #combat
        using #agility
      pure e
    _ -> LieInWait <$> liftRunMessage msg attrs
