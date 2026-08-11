module Arkham.Homebrew.DarkMatter.Stories.K2PS187CyberneticBrains (k2PS187CyberneticBrains) where

import Arkham.Homebrew.DarkMatter.CardDefs.Stories qualified as Cards
import Arkham.Homebrew.DarkMatter.Helpers (addMemories)
import Arkham.Story.Import.Lifted

newtype K2PS187CyberneticBrains = K2PS187CyberneticBrains StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

k2PS187CyberneticBrains :: StoryCard K2PS187CyberneticBrains
k2PS187CyberneticBrains = story K2PS187CyberneticBrains Cards.k2PS187CyberneticBrains

{- | "Each investigator adds 1 tally mark next to their 'Memories', regardless of
their location. Advance to act 2b. Add this card to the victory display."
-}
instance RunMessage K2PS187CyberneticBrains where
  runMessage msg s@(K2PS187CyberneticBrains attrs) = runQueueT $ case msg of
    ResolveThisStory iid (is attrs -> True) -> do
      eachInvestigator (`addMemories` 1)
      advanceCurrentAct attrs
      addToVictory iid attrs
      pure s
    _ -> K2PS187CyberneticBrains <$> liftRunMessage msg attrs
