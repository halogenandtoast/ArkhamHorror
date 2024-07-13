module Arkham.Event.Cards.MapTheArea (mapTheArea, MapTheArea (..)) where

import Arkham.Action qualified as Action
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Investigator (getMaybeLocation)
import Arkham.Helpers.Modifiers (ModifierType (..), maybeModified)
import Arkham.Helpers.SkillTest (getSkillTestInvestigator)
import Arkham.Investigate
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Placement

newtype MapTheArea = MapTheArea EventAttrs
  deriving anyclass (IsEvent, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mapTheArea :: EventCard MapTheArea
mapTheArea = event MapTheArea Cards.mapTheArea

instance HasModifiersFor MapTheArea where
  getModifiersFor SkillTestTarget (MapTheArea a) = do
    maybeModified a do
      iid <- MaybeT getSkillTestInvestigator
      lid <- MaybeT $ getMaybeLocation iid
      guard $ a.attachedTo == Just (LocationTarget lid)
      pure [Difficulty (-1)]
  getModifiersFor _ _ = pure []

instance RunMessage MapTheArea where
  runMessage msg e@(MapTheArea attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      chooseOneM iid do
        labeled "Add your {willpower}" $ skillTestModifier attrs iid (AddSkillValue #willpower)
        labeled "Add your {agility}" $ skillTestModifier attrs iid (AddSkillValue #agility)
      pushM $ setTarget attrs <$> mkInvestigate iid attrs
      pure e
    Successful (Action.Investigate, LocationTarget lid) iid _ (isTarget attrs -> True) _ -> do
      whenM (selectNone $ EventAt $ LocationWithId lid) do
        push $ PlaceEvent iid attrs.id (AttachedToLocation lid)
      pure e
    _ -> MapTheArea <$> liftRunMessage msg attrs
