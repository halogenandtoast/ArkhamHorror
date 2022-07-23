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
import Arkham.Message
import Arkham.SkillTest.Base
import Arkham.SkillTestResult
import Arkham.SkillType
import Arkham.Target

newtype OneTwoPunch = OneTwoPunch EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

oneTwoPunch :: EventCard OneTwoPunch
oneTwoPunch = event OneTwoPunch Cards.oneTwoPunch

instance RunMessage OneTwoPunch where
  runMessage msg e@(OneTwoPunch attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      pushAll
        [ skillTestModifier
          (toSource attrs)
          (InvestigatorTarget iid)
          (SkillModifier SkillCombat 1)
        , ChooseFightEnemy
          iid
          (toSource attrs)
          (Just $ toTarget attrs)
          SkillCombat
          mempty
          False
        , Discard (toTarget attrs)
        ]
      pure e
    SkillTestEnds source | isSource attrs source -> do
      mSkillTest <- getSkillTest
      case mSkillTest of
        Nothing -> error "invalid call"
        Just skillTest -> do
          let iid = eventOwner attrs
          case skillTestResult skillTest of
            SucceededBy{} -> do
              push $ chooseOne
                iid
                [ Label
                  "Fight that enemy again"
                  [ skillTestModifiers
                    (toSource attrs)
                    (InvestigatorTarget iid)
                    [SkillModifier SkillCombat 2, DamageDealt 1]
                  , ChooseFightEnemy
                    iid
                    (toSource attrs)
                    (Just $ toTarget attrs)
                    SkillCombat
                    mempty
                    False
                  , Discard (toTarget attrs)
                  ]
                , Label "Do not fight that enemy again" []
                ]
            _ -> pure ()
      pure e
    _ -> OneTwoPunch <$> runMessage msg attrs
