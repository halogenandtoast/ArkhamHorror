module Arkham.Homebrew.DarkMatter.Stories.AVoyageBeyondSpace (aVoyageBeyondSpace) where

import Arkham.Homebrew.DarkMatter.CardDefs.Stories qualified as Cards
import Arkham.Homebrew.DarkMatter.Helpers (scienceCardsControlledOrOwnedBy)
import Arkham.Story.Import.Lifted

newtype AVoyageBeyondSpace = AVoyageBeyondSpace StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aVoyageBeyondSpace :: StoryCard AVoyageBeyondSpace
aVoyageBeyondSpace = story AVoyageBeyondSpace Cards.aVoyageBeyondSpace

{- | "Each investigator gains 1 resource for each Science card they control or
own. / Remove this card from the game."
-}
instance RunMessage AVoyageBeyondSpace where
  runMessage msg s@(AVoyageBeyondSpace attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      eachInvestigator \iid -> do
        n <- scienceCardsControlledOrOwnedBy iid
        when (n > 0) $ gainResources iid attrs n
      pure s
    _ -> AVoyageBeyondSpace <$> liftRunMessage msg attrs
