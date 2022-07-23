module Arkham.Event.Cards.OneTwoPunch
  ( oneTwoPunch
  , OneTwoPunch(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
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

newtype Metadata = Metadata { isSecond :: Bool }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype OneTwoPunch = OneTwoPunch (EventAttrs `With` Metadata)
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

oneTwoPunch :: EventCard OneTwoPunch
oneTwoPunch = event (OneTwoPunch . (`with` Metadata False)) Cards.oneTwoPunch

instance RunMessage OneTwoPunch where
  runMessage msg e@(OneTwoPunch (attrs `With` metadata)) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      pushAll
        [ skillTestModifier
          (toSource attrs)
          (InvestigatorTarget iid)
          (SkillModifier SkillCombat 1)
        , ChooseFightEnemy iid (toSource attrs) Nothing SkillCombat mempty False
        ]
      pure e
    SkillTestEnds source | isSource attrs source && not (isSecond metadata) ->
      do
        mSkillTest <- getSkillTest
        case mSkillTest of
          Nothing -> error "invalid call"
          Just skillTest -> do
            let iid = eventOwner attrs
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
                        , Discard (toTarget attrs)
                        ]
                    | isStillAlive
                    ]
                  <> [ Label
                         "Do not fight that enemy again"
                         [Discard (toTarget attrs)]
                     ]
              _ -> pure ()
        pure . OneTwoPunch $ attrs `with` Metadata True
    _ -> OneTwoPunch . (`with` metadata) <$> runMessage msg attrs
