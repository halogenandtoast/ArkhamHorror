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
import Arkham.Movement
import Arkham.Prelude
import Arkham.Scenarios.CurtainCall.Helpers
import Arkham.Trait

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
        $ mkAbility (proxy LocationWithAnyHorror attrs) 1 (forced $ Leaves #when You ThisLocation)
    ]

instance RunMessage TheStrangerTheShoresOfHaliEffect where
  runMessage msg e@(TheStrangerTheShoresOfHaliEffect attrs) = case msg of
    UseThisAbility iid (isProxySource attrs -> True) 1 -> do
      push $ beginSkillTest iid attrs iid #agility 2
      pure e
    FailedSkillTest _ _ (isSource attrs -> True) (Initiator (InvestigatorTarget iid)) _ _ -> do
      popMessageMatching_ \case
        MoveFrom _ iid' _ -> iid' == iid
        _ -> False
      popMessageMatching_ \case
        MoveTo movement -> moveTarget movement == toTarget iid
        _ -> False
      push $ assignDamage iid attrs 1
      pure e
    _ -> TheStrangerTheShoresOfHaliEffect <$> runMessage msg attrs
