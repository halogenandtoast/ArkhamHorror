module Arkham.Homebrew.CircusExMortis.Stories.SilenceThePipes (silenceThePipes) where

import Arkham.Homebrew.CircusExMortis.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.CircusExMortis.CardDefs.Stories qualified as Cards
import Arkham.Matcher
import Arkham.Story.Import.Lifted

{- | Destiny "pipes" (:202). "Flip this card over and put it into play at Mossy Glen" --
unlike Strike the Heart, the Piper of Shub-Niggurath (:202b) arrives ready.
-}
newtype SilenceThePipes = SilenceThePipes StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

silenceThePipes :: StoryCard SilenceThePipes
silenceThePipes = story SilenceThePipes Cards.silenceThePipes

instance RunMessage SilenceThePipes where
  runMessage msg s@(SilenceThePipes attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      glen <- selectJust $ locationIs Locations.mossyGlen
      createEnemyAt_ Enemies.piperOfShubNiggurath glen
      pure s
    _ -> SilenceThePipes <$> liftRunMessage msg attrs
