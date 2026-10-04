module Arkham.Homebrew.TheSymphonyOfErichZann.Stories.TrumpetersMuse (trumpetersMuse) where

import Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Assets qualified as Assets
import Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Stories qualified as Cards
import Arkham.Homebrew.TheSymphonyOfErichZann.Helpers (musePayoff, scenarioI18n)
import Arkham.I18n
import Arkham.Story.Import.Lifted

newtype TrumpetersMuse = TrumpetersMuse StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

trumpetersMuse :: StoryCard TrumpetersMuse
trumpetersMuse = story TrumpetersMuse Cards.trumpetersMuse

instance RunMessage TrumpetersMuse where
  runMessage msg s@(TrumpetersMuse attrs) = runQueueT $ case msg of
    ResolveThisStory iid (is attrs -> True) -> do
      scenarioI18n $ scope "muse" $ musePayoff iid Enemies.arnoldWalker Assets.walkersTrumpet
      pure s
    _ -> TrumpetersMuse <$> liftRunMessage msg attrs
