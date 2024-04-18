module Arkham.Act.Cards.TheStrangerTheShoresOfHali (
  TheStrangerTheShoresOfHali (..),
  theStrangerTheShoresOfHali,
  theStrangerTheShoresOfHaliEffect,
) where

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
import Arkham.Trait
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
  runMessage msg a@(TheStrangerTheShoresOfHali attrs) = case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      push $ advancedWithOther attrs
      pure a
    AdvanceAct aid _ _ | aid == toId attrs && onSide B attrs -> do
      moveTheManInThePalidMaskToLobbyInsteadOfDiscarding
      privateLocations <- selectTargets $ LocationWithTrait Private
      card <- flipCard <$> genCard (toCardDef attrs)
      pushAll
        $ [AddChaosToken ElderThing, AddChaosToken ElderThing]
        <> [PlaceHorror (toSource attrs) l 1 | l <- privateLocations]
        <> [ createCardEffect Cards.theStrangerTheShoresOfHali Nothing attrs attrs
           , PlaceNextTo ActDeckTarget [card]
           , advanceActDeck attrs
           ]
      pure a
    _ -> TheStrangerTheShoresOfHali <$> runMessage msg attrs

newtype TheStrangerTheShoresOfHaliEffect = TheStrangerTheShoresOfHaliEffect EffectAttrs
  deriving anyclass (IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theStrangerTheShoresOfHaliEffect :: EffectArgs -> TheStrangerTheShoresOfHaliEffect
theStrangerTheShoresOfHaliEffect = cardEffect TheStrangerTheShoresOfHaliEffect Cards.theStrangerTheShoresOfHali

instance HasAbilities TheStrangerTheShoresOfHaliEffect where
  getAbilities (TheStrangerTheShoresOfHaliEffect attrs) =
    [ playerLimit PerRound
        $ mkAbility (proxied LocationWithAnyHorror attrs) 1 (forced $ Leaves #when You ThisLocation)
    ]

instance RunMessage TheStrangerTheShoresOfHaliEffect where
  runMessage msg e@(TheStrangerTheShoresOfHaliEffect attrs) = case msg of
    UseCardAbility iid p@(isProxySource attrs -> True) 1 (getBatchId -> batchId) _ -> do
      push $ beginSkillTest iid (AbilitySource p 1) (BatchTarget batchId) #agility (Fixed 2)
      pure e
    FailedSkillTest iid _ (isProxyAbilitySource attrs 1 -> True) Initiator {} _ _ -> do
      mtarget <- getSkillTestTarget
      case mtarget of
        Just (BatchTarget batchId) -> pushAll [assignDamage iid attrs 1, IgnoreBatch batchId]
        _ -> error "Invalid target"
      pure e
    _ -> TheStrangerTheShoresOfHaliEffect <$> runMessage msg attrs
