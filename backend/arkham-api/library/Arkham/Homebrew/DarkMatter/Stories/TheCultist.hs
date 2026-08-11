module Arkham.Homebrew.DarkMatter.Stories.TheCultist (theCultist) where

import Arkham.Homebrew.DarkMatter.CardDefs.Stories qualified as Cards
import Arkham.Homebrew.DarkMatter.Helpers (addImpendingDoom, getMemories, returnToScanningDeck)
import Arkham.Story.Import.Lifted

newtype TheCultist = TheCultist StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theCultist :: StoryCard TheCultist
theCultist = story TheCultist Cards.theCultist

{- | "If you have 4 or more 'Memories': Add 1 tally mark under Impending Doom in
your Campaign Log. Add this card to the victory display. / Otherwise: Shuffle
this card back into the scanning deck."
-}
instance RunMessage TheCultist where
  runMessage msg s@(TheCultist attrs) = runQueueT $ case msg of
    ResolveThisStory iid (is attrs -> True) -> do
      memories <- getMemories iid
      if memories >= 4
        then do
          addImpendingDoom 1
          addToVictory iid attrs
        else returnToScanningDeck attrs
      pure s
    _ -> TheCultist <$> liftRunMessage msg attrs
