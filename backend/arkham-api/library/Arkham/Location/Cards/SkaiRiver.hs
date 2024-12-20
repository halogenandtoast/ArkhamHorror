module Arkham.Location.Cards.SkaiRiver (skaiRiver) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.SkillTest.Target
import Arkham.Helpers.Story
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Story.Cards qualified as Story
import Arkham.Window (getBatchId)

newtype SkaiRiver = SkaiRiver LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

skaiRiver :: LocationCard SkaiRiver
skaiRiver = locationWith SkaiRiver Cards.skaiRiver 2 (Static 0) (canBeFlippedL .~ True)

instance HasAbilities SkaiRiver where
  getAbilities (SkaiRiver x) =
    extendRevealed1 x
      $ skillTestAbility
      $ restricted x 1 (exists $ be x <> LocationCanBeFlipped)
      $ forced
      $ Leaves #when You (be x)

instance RunMessage SkaiRiver where
  runMessage msg l@(SkaiRiver attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (getBatchId -> batchId) _ -> do
      sid <- getRandom
      chooseOneM iid do
        for_ [#willpower, #agility] \sType ->
          skillLabeled sType $ beginSkillTest sid iid (attrs.ability 1) batchId sType (Fixed 2)
      pure l
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      getSkillTestTarget >>= \case
        Just (BatchTarget batchId) -> do
          -- the story should "technically" cancel the batch, but it is easier to do here
          pushAll [CancelBatch batchId, Flip iid (attrs.ability 1) (toTarget attrs)]
        _ -> error "Invalid target"
      pure l
    Flip iid _ (isTarget attrs -> True) -> do
      readStory iid (toId attrs) Story.dreamlikeHorrors
      pure . SkaiRiver $ attrs & canBeFlippedL .~ False
    _ -> SkaiRiver <$> liftRunMessage msg attrs
