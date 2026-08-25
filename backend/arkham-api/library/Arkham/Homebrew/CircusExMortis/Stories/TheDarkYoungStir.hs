module Arkham.Homebrew.CircusExMortis.Stories.TheDarkYoungStir (theDarkYoungStir) where

import Arkham.Homebrew.CircusExMortis.CardDefs.Stories qualified as Cards
import Arkham.Story.Import.Lifted

{- | A reference card. Its Forced ability is what every "reveal a fury token"
instruction means, and that resolution is procedural rather than windowed, so it
lives in 'Arkham.Homebrew.CircusExMortis.Helpers.revealFuryToken' and the cards
that say "reveal a fury token" call it directly. This story sits next to the
fury bag purely so the table can read the direction table off it.
-}
newtype TheDarkYoungStir = TheDarkYoungStir StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theDarkYoungStir :: StoryCard TheDarkYoungStir
theDarkYoungStir = persistStory $ story TheDarkYoungStir Cards.theDarkYoungStir

instance RunMessage TheDarkYoungStir where
  runMessage msg (TheDarkYoungStir attrs) = TheDarkYoungStir <$> runMessage msg attrs
