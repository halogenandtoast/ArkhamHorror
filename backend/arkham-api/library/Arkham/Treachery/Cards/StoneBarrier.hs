module Arkham.Treachery.Cards.StoneBarrier (stoneBarrier, StoneBarrier (..)) where

import Arkham.Ability
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Placement
import Arkham.Placement
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype StoneBarrier = StoneBarrier TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stoneBarrier :: TreacheryCard StoneBarrier
stoneBarrier = treachery StoneBarrier Cards.stoneBarrier

instance HasAbilities StoneBarrier where
  getAbilities (StoneBarrier x) =
    [skillTestAbility $ restricted x 1 OnSameLocation actionAbility]

instance HasModifiersFor StoneBarrier where
  getModifiersFor (StoneBarrier a) = case a.placement of
    AttachedToLocation lid -> modifySelectWhen a a.ready (investigatorAt lid) [CannotMove]
    _ -> pure mempty

instance RunMessage StoneBarrier where
  runMessage msg t@(StoneBarrier attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      ls <- select $ NearestLocationTo iid $ LocationWithoutTreachery (treacheryIs Cards.stoneBarrier)
      chooseTargetM iid ls \lid -> do
        isFlooded <- lid <=~> FloodedLocation
        place attrs $ AttachedToLocation lid
        when isFlooded $ gainSurge attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      chooseOneM iid do
        skillLabeled #agility $ beginSkillTest sid iid attrs iid #agility (Fixed 1)
        skillLabeled #combat $ beginSkillTest sid iid attrs iid #combat (Fixed 2)
        skillLabeled #intellect $ beginSkillTest sid iid attrs iid #intellect (Fixed 3)
      pure t
    PassedThisSkillTest _iid (isSource attrs -> True) -> do
      exhaustThis attrs
      pure t
    _ -> StoneBarrier <$> liftRunMessage msg attrs
