module Arkham.Homebrew.TheSymphonyOfErichZann.Stories.PercussionistsMuse (percussionistsMuse) where

import Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Assets qualified as Assets
import Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Stories qualified as Cards
import Arkham.Homebrew.TheSymphonyOfErichZann.Helpers (musePayoff, scenarioI18n)
import Arkham.I18n
import Arkham.Story.Import.Lifted

newtype PercussionistsMuse = PercussionistsMuse StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

percussionistsMuse :: StoryCard PercussionistsMuse
percussionistsMuse = story PercussionistsMuse Cards.percussionistsMuse

instance RunMessage PercussionistsMuse where
  runMessage msg s@(PercussionistsMuse attrs) = runQueueT $ case msg of
    ResolveThisStory iid (is attrs -> True) -> do
      scenarioI18n $ scope "muse" $ musePayoff iid Enemies.songYin Assets.yinsDrumsticks
      pure s
    _ -> PercussionistsMuse <$> liftRunMessage msg attrs
