module Arkham.Homebrew.DarkMatter.Stories.CuriousDiscovery (curiousDiscovery) where

import Arkham.Homebrew.DarkMatter.CardDefs.Stories qualified as Cards
import Arkham.Homebrew.DarkMatter.Helpers (scienceCardsControlledOrOwnedBy)
import Arkham.Story.Import.Lifted

newtype CuriousDiscovery = CuriousDiscovery StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

curiousDiscovery :: StoryCard CuriousDiscovery
curiousDiscovery = story CuriousDiscovery Cards.curiousDiscovery

{- | "Each investigator draws 1 card for each Science card they control or own.
(Max 3 cards per investigator) / Remove this card from the game."
-}
instance RunMessage CuriousDiscovery where
  runMessage msg s@(CuriousDiscovery attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      eachInvestigator \iid -> do
        n <- min 3 <$> scienceCardsControlledOrOwnedBy iid
        when (n > 0) $ drawCards iid attrs n
      pure s
    _ -> CuriousDiscovery <$> liftRunMessage msg attrs
