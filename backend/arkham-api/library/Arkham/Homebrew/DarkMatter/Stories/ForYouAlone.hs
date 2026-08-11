module Arkham.Homebrew.DarkMatter.Stories.ForYouAlone (forYouAlone) where

import Arkham.Helpers.Query (getSetAsideCard)
import Arkham.Homebrew.DarkMatter.CardDefs.Assets qualified as Assets
import Arkham.Homebrew.DarkMatter.CardDefs.Stories qualified as Cards
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Story.Import.Lifted

newtype ForYouAlone = ForYouAlone StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

forYouAlone :: StoryCard ForYouAlone
forYouAlone = story ForYouAlone Cards.forYouAlone

{- | "Search for the set-aside Bottle of Whispers weakness and add it to any
investigator's hand. This card is added to their deck and does not count towards
that investigator's deck size. Add this card to the victory display."
-}
instance RunMessage ForYouAlone where
  runMessage msg s@(ForYouAlone attrs) = runQueueT $ case msg of
    ResolveThisStory iid (is attrs -> True) -> do
      bottle <- getSetAsideCard Assets.bottleOfWhispers
      investigators <- select UneliminatedInvestigator
      chooseTargetM iid investigators \bearer -> addToHand bearer [bottle]
      addToVictory iid attrs
      pure s
    _ -> ForYouAlone <$> liftRunMessage msg attrs
