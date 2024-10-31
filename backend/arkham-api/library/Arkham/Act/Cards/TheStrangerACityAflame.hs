module Arkham.Act.Cards.TheStrangerACityAflame (TheStrangerACityAflame (..), theStrangerACityAflame, theStrangerACityAflameEffect) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Card
import Arkham.ChaosToken
import Arkham.Classes
import Arkham.Effect.Runner
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Matcher hiding (Discarded)
import Arkham.Prelude
import Arkham.Scenarios.CurtainCall.Helpers

newtype TheStrangerACityAflame = TheStrangerACityAflame ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theStrangerACityAflame :: ActCard TheStrangerACityAflame
theStrangerACityAflame =
  act (2, A) TheStrangerACityAflame Cards.theStrangerACityAflame Nothing

instance HasAbilities TheStrangerACityAflame where
  getAbilities (TheStrangerACityAflame a) =
    [ mkAbility a 1
        $ Objective
        $ forced
        $ EnemyWouldBeDiscarded #when
        $ enemyIs Enemies.theManInThePallidMask
    ]

instance RunMessage TheStrangerACityAflame where
  runMessage msg a@(TheStrangerACityAflame attrs) = case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      push $ AdvanceAct (toId attrs) (attrs.ability 1) AdvancedWithOther
      pure a
    AdvanceAct aid _ _ | aid == toId attrs && onSide B attrs -> do
      theatre <- selectJust (LocationWithTitle "Theatre")
      card <- flipCard <$> genCard (toCardDef attrs)
      enabled <- createCardEffect Cards.theStrangerACityAflame Nothing attrs attrs
      pushAll
        [ AddChaosToken Cultist
        , AddChaosToken Cultist
        , PlaceHorror (toSource attrs) (toTarget theatre) 1
        , PlaceNextTo ActDeckTarget [card]
        , enabled
        , advanceActDeck attrs
        ]
      moveTheManInThePalidMaskToLobbyInsteadOfDiscarding
      pure a
    _ -> TheStrangerACityAflame <$> runMessage msg attrs

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
        $ OrWindowMatcher
          [ Enters #after You ThisLocation
          , TurnEnds #when (You <> InvestigatorAt ThisLocation)
          ]
    ]

instance RunMessage TheStrangerACityAflameEffect where
  runMessage msg e@(TheStrangerACityAflameEffect attrs) = case msg of
    UseThisAbility iid p@(isProxySource attrs -> True) 1 -> do
      sid <- getRandom
      push $ beginSkillTest sid iid (AbilitySource p 1) attrs #agility (Fixed 3)
      pure e
    FailedThisSkillTest iid (isProxyAbilitySource attrs 1 -> True) -> do
      push $ assignDamage iid attrs.source 1
      pure e
    _ -> TheStrangerACityAflameEffect <$> runMessage msg attrs
