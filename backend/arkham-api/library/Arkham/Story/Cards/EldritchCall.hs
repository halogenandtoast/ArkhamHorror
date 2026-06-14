module Arkham.Story.Cards.EldritchCall (eldritchCall) where

import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype EldritchCall = EldritchCall StoryAttrs
  deriving anyclass (IsStory, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

eldritchCall :: StoryCard EldritchCall
eldritchCall = story EldritchCall Cards.eldritchCall

-- TODO: behavior for the Cthulhu deck / storm-deck card (no engine support yet).
instance RunMessage EldritchCall where
  runMessage msg (EldritchCall attrs) = runQueueT $ EldritchCall <$> liftRunMessage msg attrs
