module Arkham.Event.Cards.Lucky where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Modifiers
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Message
import Arkham.SkillTest.Base
import Arkham.Target

newtype Lucky = Lucky EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lucky :: EventCard Lucky
lucky = event Lucky Cards.lucky

instance RunMessage Lucky where
  runMessage msg e@(Lucky attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == eventId attrs -> do
      mSkillTest <- getSkillTest
      case mSkillTest of
        Nothing -> error "invalid call"
        Just skillTest -> do
          let skillType = skillTestSkillType skillTest
          pushAll
            [ skillTestModifier
              (toSource attrs)
              (InvestigatorTarget iid)
              (SkillModifier skillType 2)
            , RerunSkillTest
            ]
      pure e
    _ -> Lucky <$> runMessage msg attrs
