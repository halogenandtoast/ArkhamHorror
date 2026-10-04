module Arkham.Homebrew.TheSymphonyOfErichZann.Stories.PianistsMuse (pianistsMuse) where

import Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Assets qualified as Assets
import Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Stories qualified as Cards
import Arkham.Homebrew.TheSymphonyOfErichZann.Helpers (musePayoff, scenarioI18n)
import Arkham.I18n
import Arkham.Story.Import.Lifted

newtype PianistsMuse = PianistsMuse StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

pianistsMuse :: StoryCard PianistsMuse
pianistsMuse = story PianistsMuse Cards.pianistsMuse

instance RunMessage PianistsMuse where
  runMessage msg s@(PianistsMuse attrs) = runQueueT $ case msg of
    ResolveThisStory iid (is attrs -> True) -> do
      scenarioI18n $ scope "muse" $ musePayoff iid Enemies.isabelLaFratta Assets.laFrattasPianoKey
      pure s
    _ -> PianistsMuse <$> liftRunMessage msg attrs
