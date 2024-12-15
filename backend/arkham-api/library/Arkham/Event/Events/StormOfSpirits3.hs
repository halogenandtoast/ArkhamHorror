module Arkham.Event.Events.StormOfSpirits3 (stormOfSpirits3, stormOfSpirits3Effect, StormOfSpirits3 (..)) where

import Arkham.Action qualified as Action
import Arkham.Aspect hiding (aspect)
import Arkham.ChaosToken
import Arkham.Classes
import Arkham.Classes.HasQueue (evalQueueT)
import Arkham.DamageEffect
import Arkham.Effect.Import
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Fight
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Helpers.SkillTest (getSkillTestTarget)
import Arkham.Matcher hiding (AttackDamageEffect, RevealChaosToken)
import Arkham.Window qualified as Window

newtype StormOfSpirits3 = StormOfSpirits3 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stormOfSpirits3 :: EventCard StormOfSpirits3
stormOfSpirits3 = event StormOfSpirits3 Cards.stormOfSpirits3

instance RunMessage StormOfSpirits3 where
  runMessage msg e@(StormOfSpirits3 attrs) = runQueueT $ case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      sid <- getRandom
      skillTestModifier sid attrs iid (SkillModifier #willpower 2)
      createCardEffect Cards.stormOfSpirits3 Nothing attrs iid
      aspect iid attrs (#willpower `InsteadOf` #combat) (setTarget attrs <$> mkChooseFight sid iid attrs)
      pure e
    Successful (Action.Fight, EnemyTarget eid) iid _ target _ | isTarget attrs target -> do
      eids <- select $ enemyAtLocationWith iid
      for_ eids \eid' -> do
        push
          $ if eid == eid'
            then EnemyDamage eid' $ delayDamage $ attack attrs 3
            else EnemyDamage eid' $ delayDamage $ isDirect $ attack attrs 3
      for_ eids $ checkDefeated attrs
      pure e
    _ -> StormOfSpirits3 <$> liftRunMessage msg attrs

newtype StormOfSpirits3Effect = StormOfSpirits3Effect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stormOfSpirits3Effect :: EffectArgs -> StormOfSpirits3Effect
stormOfSpirits3Effect = cardEffect StormOfSpirits3Effect Cards.stormOfSpirits3

instance RunMessage StormOfSpirits3Effect where
  runMessage msg e@(StormOfSpirits3Effect attrs) = runQueueT $ case msg of
    RevealChaosToken _ iid token | isTarget iid attrs.target -> do
      let triggers = chaosTokenFace token `elem` [Skull, Cultist, Tablet, ElderThing, AutoFail]
      when triggers $ do
        enemy <- fromJustNote "must be enemy" . ((.enemy) =<<) <$> getSkillTestTarget
        iids <- select $ InvestigatorAt $ locationWithEnemy enemy
        msgs <- evalQueueT $ for_ iids \iid' -> assignDamage iid' attrs.source 2
        push $ If (Window.RevealChaosTokenEffect iid token attrs.id) msgs
        disable attrs
      pure e
    SkillTestEnds _ _ _ -> disableReturn e
    _ -> StormOfSpirits3Effect <$> liftRunMessage msg attrs
