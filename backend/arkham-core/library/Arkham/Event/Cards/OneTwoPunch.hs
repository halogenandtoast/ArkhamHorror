module Arkham.Event.Cards.OneTwoPunch (
  oneTwoPunch,
  OneTwoPunch (..),
  oneTwoPunchEffect,
  OneTwoPunchEffect (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Effect.Runner ()
import Arkham.Effect.Types
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.SkillTest.Base
import Arkham.SkillTestResult

newtype Metadata = Metadata {isFirst :: Bool}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype OneTwoPunch = OneTwoPunch (EventAttrs `With` Metadata)
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

oneTwoPunch :: EventCard OneTwoPunch
oneTwoPunch = event (OneTwoPunch . (`with` Metadata True)) Cards.oneTwoPunch

instance RunMessage OneTwoPunch where
  runMessage msg e@(OneTwoPunch (attrs `With` metadata)) = case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      pushAll
        [ skillTestModifier attrs iid (SkillModifier #combat 1)
        , chooseFightEnemy iid attrs #combat
        ]
      pure e
    PassedThisSkillTest iid (isSource attrs -> True) | isFirst metadata -> do
      push $ createCardEffect Cards.oneTwoPunch Nothing attrs iid
      pure . OneTwoPunch $ attrs `with` Metadata False
    _ -> OneTwoPunch . (`with` metadata) <$> runMessage msg attrs

newtype OneTwoPunchEffect = OneTwoPunchEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

oneTwoPunchEffect :: EffectArgs -> OneTwoPunchEffect
oneTwoPunchEffect = cardEffect OneTwoPunchEffect Cards.oneTwoPunch

instance HasModifiersFor OneTwoPunchEffect where
  getModifiersFor target (OneTwoPunchEffect attrs) | attrs.target == target && effectFinished attrs = do
    pure $ toModifiers attrs [SkillModifier #combat 2, DamageDealt 1]
  getModifiersFor _ _ = pure []

instance RunMessage OneTwoPunchEffect where
  runMessage msg e@(OneTwoPunchEffect attrs@EffectAttrs {..}) = case msg of
    SkillTestEnds _ _ | effectFinished -> do
      push $ disable attrs
      pure e
    SkillTestEnds _ _ | not effectFinished -> do
      mSkillTest <- getSkillTest
      case (mSkillTest, effectTarget) of
        (Just skillTest, InvestigatorTarget iid) -> do
          case (skillTestResult skillTest, skillTestTarget skillTest) of
            (SucceededBy {}, EnemyTarget eid) -> do
              isStillAlive <- selectAny $ EnemyWithId eid
              player <- getPlayer iid
              push
                $ chooseOrRunOne player
                $ [ Label "Fight that enemy again" [FightEnemy iid eid attrs.source Nothing #combat False]
                  | isStillAlive
                  ]
                <> [Label "Do not fight that enemy again" [disable attrs]]
            _ -> pure ()
        (_, _) -> error "invalid call"
      pure . OneTwoPunchEffect $ attrs & finishedL .~ True
    _ -> OneTwoPunchEffect <$> runMessage msg attrs
