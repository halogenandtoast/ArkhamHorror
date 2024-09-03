module Arkham.Location.Cards.SeaOfBones (seaOfBones, SeaOfBones (..)) where

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
seaOfBones = location SeaOfBones Cards.seaOfBones 2 (PerPlayer 1)

instance HasAbilities SeaOfBones where
  getAbilities (SeaOfBones attrs) =
    let restriction = if locationCanBeFlipped attrs then NoRestriction else Never
     in extendRevealed
          attrs
          [restrictedAbility attrs 1 restriction $ forced $ DiscoverClues #after You (be attrs) AnyValue]

instance RunMessage SeaOfBones where
  runMessage msg l@(SeaOfBones attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      flipOver iid attrs
      pure l
    Flip iid _ (isTarget attrs -> True) -> do
      readStory iid (toId attrs) Story.somethingBelow
      pure . SeaOfBones $ attrs & canBeFlippedL .~ False
    _ -> SeaOfBones <$> liftRunMessage msg attrs
