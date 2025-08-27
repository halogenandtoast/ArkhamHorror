module Arkham.Message.Lifted.Story where

import Arkham.Card
import Arkham.Helpers.Message
import Arkham.Id
import Arkham.Message.Lifted.Queue
import Arkham.Placement
import Arkham.Prelude
import Arkham.Target

resolveStory
  :: (ReverseQueue m, ToId investigator InvestigatorId, IsCard card) => investigator -> card -> m ()
resolveStory investigator card =
  push $ ReadStory (asId investigator) (toCard card) ResolveIt Nothing

resolveStoryWithTarget
  :: (ReverseQueue m, ToId investigator InvestigatorId, IsCard card, Targetable target)
  => investigator -> card -> target -> m ()
resolveStoryWithTarget investigator card target =
  push $ ReadStory (asId investigator) (toCard card) ResolveIt (Just $ toTarget target)

resolveStoryWithPlacement
  :: (ReverseQueue m, ToId investigator InvestigatorId, IsCard card, IsPlacement placement)
  => investigator -> card -> placement -> m ()
resolveStoryWithPlacement investigator card placement =
  push
    $ ReadStoryWithPlacement (asId investigator) (toCard card) ResolveIt Nothing (toPlacement placement)
