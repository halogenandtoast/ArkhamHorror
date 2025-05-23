module Arkham.Event.Events.MapTheArea (mapTheArea) where

import Arkham.Action qualified as Action
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Investigator (getMaybeLocation)
import Arkham.Helpers.Modifiers (ModifierType (..), maybeModified_)
import Arkham.Helpers.SkillTest (getSkillTest)
import Arkham.Helpers.SkillTest.Lifted (investigateEdit_)
import Arkham.Matcher

newtype MapTheArea = MapTheArea EventAttrs
  deriving anyclass (IsEvent, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mapTheArea :: EventCard MapTheArea
mapTheArea = event MapTheArea Cards.mapTheArea

instance HasModifiersFor MapTheArea where
  getModifiersFor (MapTheArea a) =
    getSkillTest >>= traverse_ \st -> do
      maybeModified_ a st do
        lid <- MaybeT $ getMaybeLocation st.investigator
        guard $ a.attachedTo.location == Just lid
        pure [Difficulty (-1)]

instance RunMessage MapTheArea where
  runMessage msg e@(MapTheArea attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      sid <- getRandom
      chooseOneM iid do
        labeled "Add your {willpower}" $ skillTestModifier sid attrs iid (AddSkillValue #willpower)
        labeled "Add your {agility}" $ skillTestModifier sid attrs iid (AddSkillValue #agility)
      investigateEdit_ sid iid attrs (setTarget attrs)
      pure e
    Successful (Action.Investigate, LocationTarget lid) _iid _ (isTarget attrs -> True) _ -> do
      whenNone (EventAt (LocationWithId lid) <> eventIs Cards.mapTheArea) do
        whenM (lid <=~> LocationCanHaveAttachments) do
          place attrs.id (AttachedToLocation lid)
      pure e
    _ -> MapTheArea <$> liftRunMessage msg attrs
