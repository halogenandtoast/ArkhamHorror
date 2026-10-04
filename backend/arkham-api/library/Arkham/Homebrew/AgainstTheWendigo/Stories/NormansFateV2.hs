module Arkham.Homebrew.AgainstTheWendigo.Stories.NormansFateV2 (normansFateV2) where

import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Stories qualified as Cards
import Arkham.Matcher
import Arkham.Story.Import.Lifted

newtype NormansFateV2 = NormansFateV2 StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

normansFateV2 :: StoryCard NormansFateV2
normansFateV2 = story NormansFateV2 Cards.normansFateV2

{- | Part one, read when the act draws this card from the Students' Fate deck:
"Reveal the <location>. Put this card aside, without reading the second part,
until a {reaction} trigger allows you to read the second part."

Part two is read by that location when it runs out of clues; it records the
student's fate and flips this card to whatever became of them.
-}
instance RunMessage NormansFateV2 where
  runMessage msg s@(NormansFateV2 attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      selectForMaybeM (locationIs Locations.swamp) reveal
      pure s
    _ -> NormansFateV2 <$> liftRunMessage msg attrs
