module Arkham.Investigator.Cards.KymaniJones (kymaniJones, kymaniJonesEffect, KymaniJones (..)) where

import Arkham.Ability
import Arkham.Effect.Import
import Arkham.Enemy.Types (Field (..))
import Arkham.Helpers.SkillTest (getSkillTestTarget)
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Import.Lifted
import Arkham.Matcher hiding (SkillTestEnded)
import Arkham.Modifier
import Arkham.Projection

newtype KymaniJones = KymaniJones InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

kymaniJones :: InvestigatorCard KymaniJones
kymaniJones =
  investigator KymaniJones Cards.kymaniJones
    $ Stats {health = 8, sanity = 6, willpower = 3, intellect = 2, combat = 2, agility = 5}

instance HasAbilities KymaniJones where
  getAbilities (KymaniJones x) =
    [ restrictedAbility x 1 (Self <> exists (CanEngageEnemy (toSource x) <> ExhaustedEnemy))
        $ FastAbility Free
    , restrictedAbility x 2 Self
        $ freeReaction
        $ AttemptToEvade #when You (NonEliteEnemy <> ExhaustedEnemy)
    ]

instance HasChaosTokenValue KymaniJones where
  getChaosTokenValue iid ElderSign (KymaniJones attrs) | iid == toId attrs = do
    pure $ ChaosTokenValue ElderSign (PositiveModifier 1)
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage KymaniJones where
  runMessage msg i@(KymaniJones attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      enemies <- select $ CanEngageEnemy (attrs.ability 1) <> ExhaustedEnemy
      chooseOne iid [targetLabel enemy [EngageEnemy iid enemy Nothing False] | enemy <- enemies]
      pure i
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      skillTestModifier (attrs.ability 1) iid (AddSkillValue #intellect)
      createCardEffect Cards.kymaniJones Nothing (attrs.ability 1) iid
      pure i
    ResolveChaosToken _ ElderSign iid | attrs `is` iid -> do
      whenAny (ExhaustedEnemy <> enemyAtLocationWith iid) $ push PassSkillTest
      pure i
    _ -> KymaniJones <$> liftRunMessage msg attrs

newtype KymaniJonesEffect = KymaniJonesEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

kymaniJonesEffect :: EffectArgs -> KymaniJonesEffect
kymaniJonesEffect = cardEffect KymaniJonesEffect Cards.kymaniJones

instance RunMessage KymaniJonesEffect where
  runMessage msg e@(KymaniJonesEffect attrs) = runQueueT $ case msg of
    PassedThisSkillTestBy iid source n | source == attrs.source -> do
      getSkillTestTarget >>= \case
        Just (EnemyTarget enemy) -> do
          mx <- field EnemyRemainingHealth enemy
          when (maybe False (n >=) mx) $ toDiscardBy iid attrs.source enemy
        _ -> pure ()
      pure e
    SkillTestEnded -> disableReturn e
    SkillTestEnds {} -> disableReturn e
    _ -> KymaniJonesEffect <$> liftRunMessage msg attrs
