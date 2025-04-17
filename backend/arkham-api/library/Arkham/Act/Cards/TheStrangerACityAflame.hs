module Arkham.Act.Cards.TheStrangerACityAflame (theStrangerACityAflame, theStrangerACityAflameEffect) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Card
import Arkham.ChaosToken
import Arkham.Effect.Import
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Matcher hiding (Discarded)
import Arkham.Scenarios.CurtainCall.Helpers

newtype TheStrangerACityAflame = TheStrangerACityAflame ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theStrangerACityAflame :: ActCard TheStrangerACityAflame
theStrangerACityAflame = act (2, A) TheStrangerACityAflame Cards.theStrangerACityAflame Nothing

instance HasAbilities TheStrangerACityAflame where
  getAbilities (TheStrangerACityAflame a) =
    [ mkAbility a 1
        $ Objective
        $ forced
        $ EnemyWouldBeDiscarded #when
        $ enemyIs Enemies.theManInThePallidMask
    ]

instance RunMessage TheStrangerACityAflame where
  runMessage msg a@(TheStrangerACityAflame attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      addChaosToken Cultist
      addChaosToken Cultist
      theatre <- selectJust $ LocationWithTitle "Theatre"
      placeTokens attrs theatre #horror 1
      card <- flipCard <$> genCard (toCardDef attrs)
      push $ PlaceNextTo ActDeckTarget [card]
      createCardEffect Cards.theStrangerACityAflame Nothing attrs attrs
      moveTheManInThePalidMaskToLobbyInsteadOfDiscarding
      advanceActDeck attrs
      pure a
    _ -> TheStrangerACityAflame <$> liftRunMessage msg attrs

newtype TheStrangerACityAflameEffect = TheStrangerACityAflameEffect EffectAttrs
  deriving anyclass (IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theStrangerACityAflameEffect :: EffectArgs -> TheStrangerACityAflameEffect
theStrangerACityAflameEffect = cardEffect TheStrangerACityAflameEffect Cards.theStrangerACityAflame

instance HasAbilities TheStrangerACityAflameEffect where
  getAbilities (TheStrangerACityAflameEffect attrs) =
    [ playerLimit PerRound
        $ skillTestAbility
        $ mkAbility (proxied (LocationMatcherSource LocationWithAnyHorror) attrs) 1
        $ forced
        $ oneOf
          [ Enters #after You ThisLocation
          , TurnEnds #after $ You <> InvestigatorAt ThisLocation
          ]
    ]

instance RunMessage TheStrangerACityAflameEffect where
  runMessage msg e@(TheStrangerACityAflameEffect attrs) = runQueueT $ case msg of
    UseThisAbility iid p@(isProxySource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (AbilitySource p 1) attrs #agility (Fixed 3)
      pure e
    FailedThisSkillTest iid (isProxyAbilitySource attrs 1 -> True) -> do
      assignDamage iid attrs.source 1
      pure e
    _ -> TheStrangerACityAflameEffect <$> liftRunMessage msg attrs
