module Arkham.Act.Cards.TheStrangerHereIsMyReply (theStrangerHereIsMyReply, theStrangerHereIsMyReplyEffect) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Card
import Arkham.ChaosToken
import Arkham.Effect.Import
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Matcher
import Arkham.Scenarios.CurtainCall.Helpers

newtype TheStrangerHereIsMyReply = TheStrangerHereIsMyReply ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theStrangerHereIsMyReply :: ActCard TheStrangerHereIsMyReply
theStrangerHereIsMyReply = act (2, A) TheStrangerHereIsMyReply Cards.theStrangerHereIsMyReply Nothing

instance HasAbilities TheStrangerHereIsMyReply where
  getAbilities (TheStrangerHereIsMyReply a) =
    [ mkAbility a 1
        $ Objective
        $ forced
        $ EnemyWouldBeDiscarded #when
        $ enemyIs Enemies.theManInThePallidMask
    ]

instance RunMessage TheStrangerHereIsMyReply where
  runMessage msg a@(TheStrangerHereIsMyReply attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      moveTheManInThePalidMaskToLobbyInsteadOfDiscarding
      addChaosToken Tablet
      addChaosToken ElderThing
      balcony <- selectJust $ LocationWithTitle "Lobby"
      placeTokens attrs balcony #horror 1
      card <- flipCard <$> genCard (toCardDef attrs)
      push $ PlaceNextTo ActDeckTarget [card]
      createCardEffect Cards.theStrangerHereIsMyReply Nothing attrs attrs
      advanceActDeck attrs
      pure a
    _ -> TheStrangerHereIsMyReply <$> liftRunMessage msg attrs

newtype TheStrangerHereIsMyReplyEffect = TheStrangerHereIsMyReplyEffect EffectAttrs
  deriving anyclass (IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theStrangerHereIsMyReplyEffect :: EffectArgs -> TheStrangerHereIsMyReplyEffect
theStrangerHereIsMyReplyEffect = cardEffect TheStrangerHereIsMyReplyEffect Cards.theStrangerHereIsMyReply

instance HasAbilities TheStrangerHereIsMyReplyEffect where
  getAbilities (TheStrangerHereIsMyReplyEffect attrs) =
    [ playerLimit PerRound
        $ skillTestAbility
        $ mkAbility (proxied (LocationMatcherSource LocationWithAnyHorror) attrs) 1
        $ forced
        $ oneOf [Enters #after You ThisLocation, TurnEnds #when (You <> InvestigatorAt ThisLocation)]
    ]

instance RunMessage TheStrangerHereIsMyReplyEffect where
  runMessage msg e@(TheStrangerHereIsMyReplyEffect attrs) = runQueueT $ case msg of
    UseThisAbility iid p@(isProxySource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (AbilitySource p 1) attrs #willpower (Fixed 2)
      pure e
    FailedThisSkillTest iid (isProxyAbilitySource attrs 1 -> True) -> do
      assignHorror iid attrs.source 1
      pure e
    _ -> TheStrangerHereIsMyReplyEffect <$> liftRunMessage msg attrs
