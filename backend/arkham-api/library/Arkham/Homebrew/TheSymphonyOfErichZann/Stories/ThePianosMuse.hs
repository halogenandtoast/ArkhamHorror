module Arkham.Homebrew.TheSymphonyOfErichZann.Stories.ThePianosMuse (thePianosMuse) where

import Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Assets qualified as Assets
import Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Stories qualified as Cards
import Arkham.Homebrew.TheSymphonyOfErichZann.Helpers (offerInstrument, scenarioI18n)
import Arkham.I18n
import Arkham.Matcher
import Arkham.Story.Import.Lifted

newtype ThePianosMuse = ThePianosMuse StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

thePianosMuse :: StoryCard ThePianosMuse
thePianosMuse = story ThePianosMuse Cards.thePianosMuse

instance RunMessage ThePianosMuse where
  runMessage msg s@(ThePianosMuse attrs) = runQueueT $ case msg of
    {- The Piano offers La Fratta's Piano Key exactly as the Pianist's Muse does,
    but it is The Piano itself that goes to the victory display. -}
    ResolveThisStory iid (is attrs -> True) -> do
      scenarioI18n $ scope "muse" $ offerInstrument iid Assets.laFrattasPianoKey
      selectEach (assetIs Assets.thePiano) (addToVictory iid)
      pure s
    _ -> ThePianosMuse <$> liftRunMessage msg attrs
