module Arkham.Event.Events.MapTheArea (mapTheArea, MapTheArea (..)) where

import Arkham.Action qualified as Action
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Investigator (getMaybeLocation)
import Arkham.Helpers.Modifiers (ModifierType (..), maybeModified_)
import Arkham.Helpers.SkillTest (getSkillTest)
import Arkham.Investigate
import Arkham.Matcher
import Arkham.Placement

newtype MapTheArea = MapTheArea EventAttrs
  deriving anyclass (IsEvent, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mapTheArea :: EventCard MapTheArea
mapTheArea = event MapTheArea Cards.mapTheArea

instance HasModifiersFor MapTheArea where
  getModifiersFor (MapTheArea a) =
    getSkillTest >>= \case
      Nothing -> pure mempty
      Just st -> maybeModified_ a (SkillTestTarget st.id) do
        lid <- MaybeT $ getMaybeLocation st.investigator
        guard $ a.attachedTo == Just (LocationTarget lid)
        pure [Difficulty (-1)]

instance RunMessage MapTheArea where
  runMessage msg e@(MapTheArea attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      sid <- getRandom
      chooseOneM iid do
        labeled "Add your {willpower}" $ skillTestModifier sid attrs iid (AddSkillValue #willpower)
        labeled "Add your {agility}" $ skillTestModifier sid attrs iid (AddSkillValue #agility)
      pushM $ setTarget attrs <$> mkInvestigate sid iid attrs
      pure e
    Successful (Action.Investigate, LocationTarget lid) _iid _ (isTarget attrs -> True) _ -> do
      whenM (selectNone $ EventAt $ LocationWithId lid) do
        push $ PlaceEvent attrs.id (AttachedToLocation lid)
      pure e
    _ -> MapTheArea <$> liftRunMessage msg attrs
