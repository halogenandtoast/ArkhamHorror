-- | The runner behind a debug-authored custom story. See "Arkham.Custom.Enemy".
module Arkham.Custom.Story (CustomStory (..), customStory) where

import Arkham.Card.CardDef (CardDef)
import Arkham.Story.Import.Lifted

newtype CustomStory = CustomStory StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

customStory :: CardDef -> StoryCard CustomStory
customStory = story CustomStory

instance RunMessage CustomStory where
  runMessage msg (CustomStory attrs) = CustomStory <$> runMessage msg attrs
