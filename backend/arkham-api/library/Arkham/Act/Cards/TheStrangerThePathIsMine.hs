module Arkham.Act.Cards.TheStrangerThePathIsMine (
  theStrangerThePathIsMine,
  theStrangerThePathIsMineEffect,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Card
import Arkham.ChaosToken
import Arkham.Effect.Import
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Matcher hiding (Discarded)
import Arkham.Scenarios.CurtainCall.Helpers

newtype TheStrangerThePathIsMine = TheStrangerThePathIsMine ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theStrangerThePathIsMine :: ActCard TheStrangerThePathIsMine
theStrangerThePathIsMine =
  act (2, A) TheStrangerThePathIsMine Cards.theStrangerThePathIsMine Nothing

instance HasAbilities TheStrangerThePathIsMine where
  getAbilities (TheStrangerThePathIsMine a) =
    [ mkAbility a 1
        $ Objective
        $ forced
        $ EnemyWouldBeDiscarded #when
        $ enemyIs Enemies.theManInThePallidMask
    ]

instance RunMessage TheStrangerThePathIsMine where
  runMessage msg a@(TheStrangerThePathIsMine attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      addChaosToken Tablet
      addChaosToken Tablet

      theManInThePallidMask <- getTheManInThePallidMask
      selectOne (locationWithEnemy theManInThePallidMask) >>= traverse_ \lid -> placeTokens attrs lid #horror 1
      card <- flipCard <$> genCard (toCardDef attrs)
      push $ PlaceNextTo ActDeckTarget [card]
      createCardEffect Cards.theStrangerThePathIsMine Nothing attrs attrs
      moveTheManInThePalidMaskToLobbyInsteadOfDiscarding
      advanceActDeck attrs
      pure a
    _ -> TheStrangerThePathIsMine <$> liftRunMessage msg attrs

newtype TheStrangerThePathIsMineEffect = TheStrangerThePathIsMineEffect EffectAttrs
  deriving anyclass (IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theStrangerThePathIsMineEffect :: EffectArgs -> TheStrangerThePathIsMineEffect
theStrangerThePathIsMineEffect = cardEffect TheStrangerThePathIsMineEffect Cards.theStrangerThePathIsMine

instance HasAbilities TheStrangerThePathIsMineEffect where
  getAbilities (TheStrangerThePathIsMineEffect attrs) =
    [ skillTestAbility
        $ mkAbility (proxied (LocationMatcherSource LocationWithAnyHorror) attrs) 1
        $ forced
        $ Leaves #after You ThisLocation
    ]

instance RunMessage TheStrangerThePathIsMineEffect where
  runMessage msg e@(TheStrangerThePathIsMineEffect attrs) = runQueueT $ case msg of
    UseThisAbility iid p@(isProxySource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (AbilitySource p 1) attrs #agility (Fixed 4)
      pure e
    FailedThisSkillTest iid (isProxyAbilitySource attrs 1 -> True) -> do
      assignDamageAndHorror iid attrs.source 1 1
      pure e
    _ -> TheStrangerThePathIsMineEffect <$> liftRunMessage msg attrs
