module Arkham.Story.Cards.TracesToNowhere.SnapOutOfIt (snapOutOfIt) where

import Arkham.Scenarios.TracesToNowhere.Helpers
import Arkham.Story.CardDefs.TracesToNowhere qualified as Cards
import Arkham.Story.Import.Lifted

newtype SnapOutOfIt = SnapOutOfIt StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

snapOutOfIt :: StoryCard SnapOutOfIt
snapOutOfIt = story SnapOutOfIt Cards.snapOutOfIt

instance RunMessage SnapOutOfIt where
  runMessage msg s@(SnapOutOfIt attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      culpritParleyBegin attrs #willpower
      pure s
    PassedThisSkillTest iid (isSource attrs -> True) -> do
      culpritParleySuccess attrs iid
      pure s
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      culpritParleyFailure attrs iid
      pure s
    _ -> SnapOutOfIt <$> liftRunMessage msg attrs
