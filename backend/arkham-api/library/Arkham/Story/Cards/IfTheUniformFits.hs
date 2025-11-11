module Arkham.Story.Cards.IfTheUniformFits (ifTheUniformFits) where

import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype IfTheUniformFits = IfTheUniformFits StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ifTheUniformFits :: StoryCard IfTheUniformFits
ifTheUniformFits = story IfTheUniformFits Cards.ifTheUniformFits

instance RunMessage IfTheUniformFits where
  runMessage msg s@(IfTheUniformFits attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      pure s
    _ -> IfTheUniformFits <$> liftRunMessage msg attrs
