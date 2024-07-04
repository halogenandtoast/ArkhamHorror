module Arkham.Event.Cards.BlindingLight2 (blindingLight2, blindingLight2Effect, BlindingLight2 (..)) where

import Arkham.Action qualified as Action
import Arkham.Aspect
import Arkham.ChaosToken
import Arkham.Effect.Import
import Arkham.Evade
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Message qualified as Msg
import Arkham.Window qualified as Window

newtype BlindingLight2 = BlindingLight2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

blindingLight2 :: EventCard BlindingLight2
blindingLight2 = event BlindingLight2 Cards.blindingLight2

instance RunMessage BlindingLight2 where
  runMessage msg e@(BlindingLight2 attrs) = runQueueT $ case msg of
    PlayThisEvent iid eid | attrs `is` eid -> do
      createCardEffect Cards.blindingLight2 Nothing attrs iid
      createCardEffect Cards.blindingLight2 Nothing attrs SkillTestTarget
      pushAllM $ leftOr <$> aspect iid attrs (#willpower `InsteadOf` #agility) (mkChooseEvade iid attrs)
      pure e
    _ -> BlindingLight2 <$> liftRunMessage msg attrs

newtype BlindingLight2Effect = BlindingLight2Effect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

blindingLight2Effect :: EffectArgs -> BlindingLight2Effect
blindingLight2Effect = cardEffect BlindingLight2Effect Cards.blindingLight2

instance RunMessage BlindingLight2Effect where
  runMessage msg e@(BlindingLight2Effect attrs) = runQueueT $ case msg of
    RevealChaosToken _ iid token | InvestigatorTarget iid == attrs.target -> do
      when (token.face `elem` [Skull, Cultist, Tablet, ElderThing, AutoFail]) do
        push
          $ If
            (Window.RevealChaosTokenEffect iid token attrs.id)
            [LoseActions iid attrs.source 1, Msg.assignHorror iid attrs 1]
        disable attrs
      pure e
    PassedSkillTest iid (Just Action.Evade) _ (Initiator (EnemyTarget eid)) _ _ | SkillTestTarget == attrs.target -> do
      nonAttackEnemyDamage iid 2 eid
      disableReturn e
    SkillTestEnds _ _ -> disableReturn e
    _ -> BlindingLight2Effect <$> liftRunMessage msg attrs
