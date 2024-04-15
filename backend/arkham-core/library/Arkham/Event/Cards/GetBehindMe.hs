module Arkham.Event.Cards.GetBehindMe (getBehindMe, getBehindMeEffect, GetBehindMe (..)) where

import Arkham.Ability
import Arkham.Attack
import Arkham.Effect.Import
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Window (getAttackDetails)
import Arkham.Matcher
import Arkham.Modifier

newtype GetBehindMe = GetBehindMe EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

getBehindMe :: EventCard GetBehindMe
getBehindMe = event GetBehindMe Cards.getBehindMe

instance RunMessage GetBehindMe where
  runMessage msg e@(GetBehindMe attrs) = runQueueT $ case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      createCardEffect Cards.getBehindMe Nothing attrs iid
      pure e
    _ -> GetBehindMe <$> lift (runMessage msg attrs)

newtype GetBehindMeEffect = GetBehindMeEffect EffectAttrs
  deriving anyclass (IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

getBehindMeEffect :: EffectArgs -> GetBehindMeEffect
getBehindMeEffect = cardEffect GetBehindMeEffect Cards.getBehindMe

instance HasAbilities GetBehindMeEffect where
  getAbilities (GetBehindMeEffect x) = case x.target of
    InvestigatorTarget iid ->
      [ restrictedAbility x 1 (youExist $ InvestigatorWithId iid)
          $ SilentForcedAbility
          $ EnemyWouldAttack #when (InvestigatorAt YourLocation) AnyEnemyAttack AnyEnemy
      ]
    _ -> error "invalid target for effect"

instance RunMessage GetBehindMeEffect where
  runMessage msg e@(GetBehindMeEffect attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (getAttackDetails -> details) _ -> do
      let enemy = attackEnemy details
      enemyAttackModifier attrs iid (WillCancelHorror 1)
      pushAll
        [ ChangeEnemyAttackTarget enemy (toTarget iid)
        , AfterEnemyAttack enemy [EnemyEngageInvestigator enemy iid]
        ]
      pure e
    EndPhase -> do
      disable attrs
      pure e
    _ -> GetBehindMeEffect <$> lift (runMessage msg attrs)
