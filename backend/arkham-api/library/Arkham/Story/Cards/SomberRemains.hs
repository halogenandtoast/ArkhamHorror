module Arkham.Story.Cards.SomberRemains
  ( SomberRemains(..)
  , somberRemains
  ) where

import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype SomberRemains = SomberRemains StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

somberRemains :: StoryCard SomberRemains
somberRemains = story SomberRemains Cards.somberRemains

instance RunMessage SomberRemains where
  runMessage msg s@(SomberRemains attrs) = runQueueT $ case msg of
    ResolveStory _ ResolveIt story' | story' == toId attrs -> do
      pure s
    _ -> SomberRemains <$> liftRunMessage msg attrs
