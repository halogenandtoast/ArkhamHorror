module Arkham.Event.Events.MindOverMatter2 (MindOverMatter2, mindOverMatter2) where

import Arkham.Effect.Runner ()
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Modifier

newtype MindOverMatter2 = MindOverMatter2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mindOverMatter2 :: EventCard MindOverMatter2
mindOverMatter2 = event MindOverMatter2 Cards.mindOverMatter2

instance RunMessage MindOverMatter2 where
  runMessage msg e@(MindOverMatter2 attrs) = runQueueT $ case msg of
    PlayThisEvent iid eid | attrs `is` eid -> do
      roundModifiers
        eid
        iid
        [AddSkillToOtherSkill #intellect #combat, AddSkillToOtherSkill #intellect #agility]
      drawCardsIfCan iid attrs 1
      pure e
    _ -> MindOverMatter2 <$> liftRunMessage msg attrs
