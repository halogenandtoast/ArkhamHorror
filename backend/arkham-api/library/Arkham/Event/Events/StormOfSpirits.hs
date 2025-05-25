module Arkham.Event.Events.StormOfSpirits (stormOfSpirits) where

import Arkham.Action qualified as Action
import Arkham.Aspect hiding (aspect)
import Arkham.Classes
import Arkham.DamageEffect
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Fight
import Arkham.Matcher hiding (AttackDamageEffect, RevealChaosToken)

newtype StormOfSpirits = StormOfSpirits EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stormOfSpirits :: EventCard StormOfSpirits
stormOfSpirits = event StormOfSpirits Cards.stormOfSpirits

instance RunMessage StormOfSpirits where
  runMessage msg e@(StormOfSpirits attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      sid <- getRandom
      aspect iid attrs (#willpower `InsteadOf` #combat) (setTarget attrs <$> mkChooseFight sid iid attrs)
      pure e
    ChoseEnemy sid _ (isSource attrs -> True) _eid -> do
      let tokens = oneOf [#skull, #cultist, #tablet, #elderthing, #autofail]
      onRevealChaosTokenEffect sid tokens attrs attrs do
        doStep 1 msg
      pure e
    DoStep 1 (ChoseEnemy _sid _iid (isSource attrs -> True) eid) -> do
      selectEach (InvestigatorAt $ locationWithEnemy eid) \iid' -> assignDamage iid' attrs 1
      pure e
    Successful (Action.Fight, EnemyTarget eid) iid _ target _ | isTarget attrs target -> do
      let
        toMsg eid' =
          if eid == eid'
            then EnemyDamage eid' $ delayDamage $ attack attrs 2
            else EnemyDamage eid' $ delayDamage $ isDirect $ attack attrs 2
      eids <- select $ enemyAtLocationWith iid
      pushAll $ map toMsg eids
      for_ eids (checkDefeated attrs)
      pure e
    _ -> StormOfSpirits <$> liftRunMessage msg attrs
