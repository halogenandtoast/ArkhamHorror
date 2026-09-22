module Arkham.Event.Events.StormOfSpirits (stormOfSpirits) where

import Arkham.Action qualified as Action
import Arkham.Aspect hiding (aspect)
import Arkham.Campaigns.TheScarletKeys.Concealed.Helpers
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
      onRevealChaosTokenEffect sid tokens attrs attrs $ doStep 1 msg
      pure e
    DoStep 1 (ChoseEnemy _sid _iid (isSource attrs -> True) eid) -> do
      simultaneously $ selectEach (InvestigatorAt $ locationWithEnemy eid) \iid' -> assignDamage iid' attrs 1
      pure e
    Successful (Action.Fight, EnemyTarget eid) iid _ (isTarget attrs -> True) _ -> do
      eids <- select $ enemyAtLocationWith iid
      simultaneously $ for_ eids \eid' -> do
        let setAttack = if eid == eid' then id else isDirect
        push $ DealDamage (toTarget eid') $ delayDamage $ setAttack $ attack attrs 2
      simultaneously $ for_ eids (checkDefeated attrs)
      chooseExposeConcealed iid attrs
      pure e
    _ -> StormOfSpirits <$> liftRunMessage msg attrs
