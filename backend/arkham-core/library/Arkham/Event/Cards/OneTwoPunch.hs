module Arkham.Event.Cards.OneTwoPunch (
  oneTwoPunch,
  OneTwoPunch (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.SkillTest.Base

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
      skillTest <- fromJustNote "invalid call" <$> getSkillTest
      case skillTestTarget skillTest of
        EnemyTarget eid -> do
          isStillAlive <- selectAny $ EnemyWithId eid
          player <- getPlayer iid
          push
            $ chooseOrRunOne player
            $ [ Label
                "Fight that enemy again"
                [ BeginSkillTestWithPreMessages'
                    [ skillTestModifiers attrs iid [SkillModifier #combat 2, DamageDealt 1]
                    ]
                    (resetSkillTest skillTest)
                ]
              | isStillAlive
              ]
            <> [Label "Do not fight that enemy again" []]
        _ -> error "invalid call"
      pure . OneTwoPunch $ attrs `with` Metadata False
    _ -> OneTwoPunch . (`with` metadata) <$> runMessage msg attrs
