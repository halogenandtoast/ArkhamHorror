module Arkham.Homebrew.DarkMatter.Stories.HiddenSignals (hiddenSignals) where

import Arkham.Homebrew.DarkMatter.CardDefs.Stories qualified as Cards
import Arkham.Homebrew.DarkMatter.Helpers (scienceCardsControlledOrOwnedBy)
import Arkham.Story.Import.Lifted

newtype HiddenSignals = HiddenSignals StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hiddenSignals :: StoryCard HiddenSignals
hiddenSignals = story HiddenSignals Cards.hiddenSignals

{- | "Each investigator gains 1 clue from the token bank for each Science card
they control or own. (Max 2 clues per investigator) / Remove this card from the
game."
-}
instance RunMessage HiddenSignals where
  runMessage msg s@(HiddenSignals attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      eachInvestigator \iid -> do
        n <- min 2 <$> scienceCardsControlledOrOwnedBy iid
        when (n > 0) $ gainClues iid attrs n
      pure s
    _ -> HiddenSignals <$> liftRunMessage msg attrs
