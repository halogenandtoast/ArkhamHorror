module Arkham.Event.Cards.MindOverMatter2 (MindOverMatter2, mindOverMatter2) where

import Arkham.Classes
import Arkham.Effect.Runner ()
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Modifiers
import Arkham.Prelude

newtype MindOverMatter2 = MindOverMatter2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mindOverMatter2 :: EventCard MindOverMatter2
mindOverMatter2 = event MindOverMatter2 Cards.mindOverMatter2

instance RunMessage MindOverMatter2 where
  runMessage msg e@(MindOverMatter2 attrs) = case msg of
    PlayThisEvent iid eid | attrs `is` eid -> do
      let drawing = drawCards iid attrs 1

      pushAll
        [ roundModifiers
            eid
            iid
            [AddSkillToOtherSkill #intellect #combat, AddSkillToOtherSkill #intellect #agility]
        , drawing
        ]
      pure e
    _ -> MindOverMatter2 <$> runMessage msg attrs
