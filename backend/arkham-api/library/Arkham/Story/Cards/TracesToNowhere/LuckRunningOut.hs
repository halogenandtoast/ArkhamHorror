module Arkham.Story.Cards.TracesToNowhere.LuckRunningOut (luckRunningOut) where

import Arkham.Scenarios.TracesToNowhere.Helpers
import Arkham.Story.CardDefs.TracesToNowhere qualified as Cards
import Arkham.Story.Import.Lifted

newtype LuckRunningOut = LuckRunningOut StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

luckRunningOut :: StoryCard LuckRunningOut
luckRunningOut = story LuckRunningOut Cards.luckRunningOut

instance RunMessage LuckRunningOut where
  runMessage msg s@(LuckRunningOut attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      culpritParleyBegin attrs #agility
      pure s
    PassedThisSkillTest iid (isSource attrs -> True) -> do
      culpritParleySuccess attrs iid
      pure s
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      culpritParleyFailure attrs iid
      pure s
    _ -> LuckRunningOut <$> liftRunMessage msg attrs
