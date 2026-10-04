module Arkham.Homebrew.TheSymphonyOfErichZann.Stories.ViolinistsMuse (violinistsMuse) where

import Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Assets qualified as Assets
import Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Stories qualified as Cards
import Arkham.Homebrew.TheSymphonyOfErichZann.Helpers (musePayoff, scenarioI18n)
import Arkham.I18n
import Arkham.Story.Import.Lifted

newtype ViolinistsMuse = ViolinistsMuse StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

violinistsMuse :: StoryCard ViolinistsMuse
violinistsMuse = story ViolinistsMuse Cards.violinistsMuse

instance RunMessage ViolinistsMuse where
  runMessage msg s@(ViolinistsMuse attrs) = runQueueT $ case msg of
    ResolveThisStory iid (is attrs -> True) -> do
      scenarioI18n $ scope "muse" $ musePayoff iid Enemies.nicolePage Assets.pagesViolin
      pure s
    _ -> ViolinistsMuse <$> liftRunMessage msg attrs
