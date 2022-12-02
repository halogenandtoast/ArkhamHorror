module Arkham.Event.Cards.OneTwoPunch
  ( oneTwoPunch
  , OneTwoPunch(..)
  , oneTwoPunchEffect
  , OneTwoPunchEffect(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Effect.Types
import Arkham.Effect.Runner ()
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Message
import Arkham.SkillTest.Base
import Arkham.SkillTestResult
import Arkham.SkillType
import Arkham.Target

newtype Metadata = Metadata { isFirst :: Bool }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype OneTwoPunch = OneTwoPunch (EventAttrs `With` Metadata)
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

oneTwoPunch :: EventCard OneTwoPunch
oneTwoPunch = event (OneTwoPunch . (`with` Metadata True)) Cards.oneTwoPunch

instance RunMessage OneTwoPunch where
  runMessage msg e@(OneTwoPunch (attrs `With` metadata)) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      pushAll
        [ skillTestModifier
          (toSource attrs)
          (InvestigatorTarget iid)
          (SkillModifier SkillCombat 1)
        , ChooseFightEnemy iid (toSource attrs) Nothing SkillCombat mempty False
        , Discard (toTarget attrs)
        ]
      pure e
    PassedSkillTest iid _ source SkillTestInitiatorTarget{} _ _
      | isSource attrs source && isFirst metadata -> do
        push $ CreateEffect "60117" Nothing source (InvestigatorTarget iid)
        pure . OneTwoPunch $ attrs `with` Metadata False
    _ -> OneTwoPunch . (`with` metadata) <$> runMessage msg attrs

newtype OneTwoPunchEffect = OneTwoPunchEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

oneTwoPunchEffect :: EffectArgs -> OneTwoPunchEffect
oneTwoPunchEffect = OneTwoPunchEffect . uncurry4 (baseAttrs "60117")

instance RunMessage OneTwoPunchEffect where
  runMessage msg (OneTwoPunchEffect attrs@EffectAttrs {..}) = case msg of
    SkillTestEnds _ _ | not effectFinished -> do
      mSkillTest <- getSkillTest
      case (mSkillTest, effectTarget) of
        (Just skillTest, InvestigatorTarget iid) -> do
          case (skillTestResult skillTest, skillTestTarget skillTest) of
            (SucceededBy{}, EnemyTarget eid) -> do
              isStillAlive <- selectAny $ EnemyWithId eid
              push
                $ chooseOrRunOne iid
                $ [ Label
                      "Fight that enemy again"
                      [ skillTestModifiers
                        (toSource attrs)
                        (InvestigatorTarget iid)
                        [SkillModifier SkillCombat 2, DamageDealt 1]
                      , ChooseFightEnemy
                        iid
                        (toSource attrs)
                        Nothing
                        SkillCombat
                        mempty
                        False
                      , DisableEffect (toId attrs)
                      ]
                  | isStillAlive
                  ]
                <> [ Label
                       "Do not fight that enemy again"
                       [DisableEffect (toId attrs)]
                   ]
            _ -> pure ()
        (_, _) -> error "invalid call"
      pure . OneTwoPunchEffect $ attrs & finishedL .~ True
    _ -> OneTwoPunchEffect <$> runMessage msg attrs
