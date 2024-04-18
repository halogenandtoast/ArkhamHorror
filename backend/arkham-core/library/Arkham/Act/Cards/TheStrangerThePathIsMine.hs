module Arkham.Act.Cards.TheStrangerThePathIsMine (
  TheStrangerThePathIsMine (..),
  theStrangerThePathIsMine,
  theStrangerThePathIsMineEffect,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Card
import Arkham.ChaosToken
import Arkham.Classes
import Arkham.Effect.Runner
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
  runMessage msg a@(TheStrangerThePathIsMine attrs) = case msg of
    UseCardAbility _ source 1 _ _ | isSource attrs source -> do
      push $ AdvanceAct (toId attrs) source AdvancedWithOther
      pure a
    AdvanceAct aid _ _ | aid == toId attrs && onSide B attrs -> do
      theManInThePallidMask <- getTheManInThePallidMask
      moveTheManInThePalidMaskToLobbyInsteadOfDiscarding
      mlid <- selectOne $ LocationWithEnemy $ EnemyWithId theManInThePallidMask
      card <- flipCard <$> genCard (toCardDef attrs)
      for_ mlid $ \lid ->
        pushAll
          [ AddChaosToken Tablet
          , AddChaosToken Tablet
          , PlaceHorror (toSource attrs) (toTarget lid) 1
          , PlaceNextTo ActDeckTarget [card]
          , createCardEffect Cards.theStrangerThePathIsMine Nothing attrs attrs
          , advanceActDeck attrs
          ]
      pure a
    _ -> TheStrangerThePathIsMine <$> runMessage msg attrs

newtype TheStrangerThePathIsMineEffect = TheStrangerThePathIsMineEffect EffectAttrs
  deriving anyclass (IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theStrangerThePathIsMineEffect :: EffectArgs -> TheStrangerThePathIsMineEffect
theStrangerThePathIsMineEffect = cardEffect TheStrangerThePathIsMineEffect Cards.theStrangerThePathIsMine

instance HasAbilities TheStrangerThePathIsMineEffect where
  getAbilities (TheStrangerThePathIsMineEffect attrs) =
    [ mkAbility (proxied (LocationMatcherSource LocationWithAnyHorror) attrs) 1
        $ forced
        $ Leaves #after You ThisLocation
    ]

instance RunMessage TheStrangerThePathIsMineEffect where
  runMessage msg e@(TheStrangerThePathIsMineEffect attrs) = case msg of
    UseCardAbility iid p@(ProxySource _ source) 1 _ _ | isSource attrs source -> do
      push $ beginSkillTest iid (AbilitySource p 1) attrs #agility (Fixed 4)
      pure e
    FailedSkillTest _ _ source (Initiator (InvestigatorTarget iid)) _ _ | isProxyAbilitySource attrs 1 source -> do
      push $ assignDamageAndHorror iid source 1 1
      pure e
    _ -> TheStrangerThePathIsMineEffect <$> runMessage msg attrs
