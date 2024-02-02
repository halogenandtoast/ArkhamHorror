module Arkham.Event.Cards.Lucky where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Modifiers

newtype Lucky = Lucky EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

lucky :: EventCard Lucky
lucky = event Lucky Cards.lucky

instance RunMessage Lucky where
  runMessage msg e@(Lucky attrs) = case msg of
    PlayThisEvent iid eid | attrs `is` eid -> do
      pushAll [skillTestModifier attrs iid (AnySkillValue 2), RerunSkillTest]
      pure e
    _ -> Lucky <$> runMessage msg attrs
