module Arkham.Homebrew.DarkMatter.Stories.WhoAmI (whoAmI) where

import Arkham.Homebrew.DarkMatter.CardDefs.Stories qualified as Cards
import Arkham.Homebrew.DarkMatter.Helpers (addMemories)
import Arkham.Matcher
import Arkham.Story.Import.Lifted
import Arkham.Strategy

newtype WhoAmI = WhoAmI StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

whoAmI :: StoryCard WhoAmI
whoAmI = story WhoAmI Cards.whoAmI

{- | "Each investigator at your location adds 1 tally mark next to their
'Memories'. You may search the top 6 cards of your deck for a card and draw it.
Add this card to the victory display."
-}
instance RunMessage WhoAmI where
  runMessage msg s@(WhoAmI attrs) = runQueueT $ case msg of
    ResolveThisStory iid (is attrs -> True) -> do
      colocated <- select $ colocatedWith iid
      for_ colocated (`addMemories` 1)
      search iid attrs iid [fromTopOfDeck 6] (basic AnyCard) (DrawFound iid 1)
      addToVictory iid attrs
      pure s
    _ -> WhoAmI <$> liftRunMessage msg attrs
