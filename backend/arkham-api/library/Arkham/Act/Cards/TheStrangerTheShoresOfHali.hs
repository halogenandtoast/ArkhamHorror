module Arkham.Act.Cards.TheStrangerTheShoresOfHali (
  theStrangerTheShoresOfHali,
  theStrangerTheShoresOfHaliEffect,
) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Card
import Arkham.Helpers.SkillTest (getSkillTestTarget)
import Arkham.ChaosToken
import Arkham.Effect.Import
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Matcher hiding (Discarded)
import Arkham.Scenarios.CurtainCall.Helpers
import Arkham.Trait hiding (ElderThing)
import Arkham.Window (getBatchId)

newtype TheStrangerTheShoresOfHali = TheStrangerTheShoresOfHali ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theStrangerTheShoresOfHali :: ActCard TheStrangerTheShoresOfHali
theStrangerTheShoresOfHali =
  act (2, A) TheStrangerTheShoresOfHali Cards.theStrangerTheShoresOfHali Nothing

instance HasAbilities TheStrangerTheShoresOfHali where
  getAbilities (TheStrangerTheShoresOfHali a) =
    [ mkAbility a 1
        $ Objective
        $ forced
        $ EnemyWouldBeDiscarded #when
        $ enemyIs Enemies.theManInThePallidMask
    ]

instance RunMessage TheStrangerTheShoresOfHali where
  runMessage msg a@(TheStrangerTheShoresOfHali attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      card <- flipCard <$> genCard (toCardDef attrs)
      addChaosToken ElderThing
      addChaosToken ElderThing
      selectEach (LocationWithTrait Private) \l -> placeTokens attrs l #horror 1
      createCardEffect Cards.theStrangerTheShoresOfHali Nothing attrs attrs
      push $ PlaceNextTo ActDeckTarget [card]
      advanceActDeck attrs
      moveTheManInThePalidMaskToLobbyInsteadOfDiscarding
      pure a
    _ -> TheStrangerTheShoresOfHali <$> liftRunMessage msg attrs

newtype TheStrangerTheShoresOfHaliEffect = TheStrangerTheShoresOfHaliEffect EffectAttrs
  deriving anyclass (IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theStrangerTheShoresOfHaliEffect :: EffectArgs -> TheStrangerTheShoresOfHaliEffect
theStrangerTheShoresOfHaliEffect = cardEffect TheStrangerTheShoresOfHaliEffect Cards.theStrangerTheShoresOfHali

instance HasAbilities TheStrangerTheShoresOfHaliEffect where
  getAbilities (TheStrangerTheShoresOfHaliEffect attrs) =
    [ skillTestAbility
        $ playerLimit PerRound
        $ mkAbility (proxied LocationWithAnyHorror attrs) 1 (forced $ Leaves #when You ThisLocation)
    ]

instance RunMessage TheStrangerTheShoresOfHaliEffect where
  runMessage msg e@(TheStrangerTheShoresOfHaliEffect attrs) = runQueueT $ case msg of
    UseCardAbility iid p@(isProxySource attrs -> True) 1 (getBatchId -> batchId) _ -> do
      sid <- getRandom
      beginSkillTest sid iid (AbilitySource p 1) (BatchTarget batchId) #agility (Fixed 2)
      pure e
    FailedSkillTest iid _ (isProxyAbilitySource attrs 1 -> True) Initiator {} _ _ -> do
      getSkillTestTarget >>= \case
        Just (BatchTarget batchId) -> do
          assignDamage iid attrs 1
          push $ IgnoreBatch batchId
        _ -> error "Invalid target"
      pure e
    _ -> TheStrangerTheShoresOfHaliEffect <$> liftRunMessage msg attrs
