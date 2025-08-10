module Arkham.Location.Cards.SeaOfBones (seaOfBones) where

import Arkham.Ability
import Arkham.Helpers.Story (readStory)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Story.Cards qualified as Story

newtype SeaOfBones = SeaOfBones LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

seaOfBones :: LocationCard SeaOfBones
seaOfBones = locationWith SeaOfBones Cards.seaOfBones 2 (PerPlayer 1) (canBeFlippedL .~ True)

instance HasAbilities SeaOfBones where
  getAbilities (SeaOfBones a) =
    extendRevealed1 a $ restricted a 1 restriction $ forced $ DiscoverClues #after You (be a) AnyValue
   where
    restriction = if locationCanBeFlipped a then NoRestriction else Never

instance RunMessage SeaOfBones where
  runMessage msg l@(SeaOfBones attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      flipOver iid attrs
      pure l
    Flip iid _ (isTarget attrs -> True) -> do
      readStory iid (toId attrs) Story.somethingBelow
      pure . SeaOfBones $ attrs & canBeFlippedL .~ False
    _ -> SeaOfBones <$> liftRunMessage msg attrs
