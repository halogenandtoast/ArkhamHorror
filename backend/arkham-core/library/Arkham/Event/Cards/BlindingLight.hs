module Arkham.Event.Cards.BlindingLight where

import Arkham.Action qualified as Action
import Arkham.Aspect
import Arkham.ChaosToken
import Arkham.Effect.Import
import Arkham.Evade
import Arkham.Event.Cards qualified as Cards (blindingLight)
import Arkham.Event.Import.Lifted
import Arkham.Window qualified as Window

newtype BlindingLight = BlindingLight EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

blindingLight :: EventCard BlindingLight
blindingLight = event BlindingLight Cards.blindingLight

instance RunMessage BlindingLight where
  runMessage msg e@(BlindingLight attrs) = runQueueT $ case msg of
    PlayThisEvent iid eid | attrs `is` eid -> do
      createCardEffect Cards.blindingLight Nothing attrs iid
      createCardEffect Cards.blindingLight Nothing attrs SkillTestTarget
      pushAllM $ leftOr <$> aspect iid attrs (#willpower `InsteadOf` #agility) (mkChooseEvade iid attrs)
      pure e
    _ -> BlindingLight <$> liftRunMessage msg attrs

newtype BlindingLightEffect = BlindingLightEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

blindingLightEffect :: EffectArgs -> BlindingLightEffect
blindingLightEffect = cardEffect BlindingLightEffect Cards.blindingLight

instance RunMessage BlindingLightEffect where
  runMessage msg e@(BlindingLightEffect attrs) = runQueueT $ case msg of
    RevealChaosToken _ iid token | InvestigatorTarget iid == attrs.target -> do
      when (token.face `elem` [Skull, Cultist, Tablet, ElderThing, AutoFail]) do
        push $ If (Window.RevealChaosTokenEffect iid token attrs.id) [LoseActions iid (toSource attrs) 1]
        disable attrs
      pure e
    PassedSkillTest iid (Just Action.Evade) _ (Initiator (EnemyTarget eid)) _ _ | attrs.target == SkillTestTarget -> do
      nonAttackEnemyDamage iid 1 eid
      disableReturn e
    SkillTestEnds _ _ -> disableReturn e
    _ -> BlindingLightEffect <$> liftRunMessage msg attrs
