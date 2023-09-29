module Arkham.Event.Cards.MindOverMatter (
  MindOverMatter,
  mindOverMatter,
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Modifiers

newtype MindOverMatter = MindOverMatter EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mindOverMatter :: EventCard MindOverMatter
mindOverMatter = event MindOverMatter Cards.mindOverMatter

instance RunMessage MindOverMatter where
  runMessage msg e@(MindOverMatter attrs) = case msg of
    PlayThisEvent iid eid | attrs `is` eid -> do
      push
        $ roundModifiers eid iid [UseSkillInPlaceOf #combat #intellect, UseSkillInPlaceOf #agility #intellect]
      pure e
    _ -> MindOverMatter <$> runMessage msg attrs
