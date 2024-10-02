module Arkham.Event.Events.MindOverMatter (MindOverMatter, mindOverMatter) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Modifier

newtype MindOverMatter = MindOverMatter EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mindOverMatter :: EventCard MindOverMatter
mindOverMatter = event MindOverMatter Cards.mindOverMatter

instance RunMessage MindOverMatter where
  runMessage msg e@(MindOverMatter attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      roundModifiers attrs iid [UseSkillInPlaceOf sType #intellect | sType <- [#combat, #agility]]
      pure e
    _ -> MindOverMatter <$> liftRunMessage msg attrs
