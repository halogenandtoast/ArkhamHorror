module Arkham.Event.Cards.Lucky2 where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Modifiers
import Arkham.Message
import Arkham.SkillTest.Base
import Arkham.Target

newtype Lucky2 = Lucky2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lucky2 :: EventCard Lucky2
lucky2 = event Lucky2 Cards.lucky2

instance RunMessage Lucky2 where
  runMessage msg e@(Lucky2 attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == eventId attrs -> do
      mSkillTest <- getSkillTest
      case mSkillTest of
        Nothing -> error "invalid call"
        Just skillTest -> do
          let skillType = skillTestSkillType skillTest
          drawing <- drawCards iid attrs 1
          pushAll
            [ discard attrs
            , drawing
            , skillTestModifier
              (toSource attrs)
              (InvestigatorTarget iid)
              (SkillModifier skillType 2)
            , RerunSkillTest
            ]
      pure e
    _ -> Lucky2 <$> runMessage msg attrs
