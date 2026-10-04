module Arkham.Homebrew.AgainstTheWendigo.Stories.SylviasFateV1 (sylviasFateV1) where

import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Stories qualified as Cards
import Arkham.Matcher
import Arkham.Story.Import.Lifted

newtype SylviasFateV1 = SylviasFateV1 StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sylviasFateV1 :: StoryCard SylviasFateV1
sylviasFateV1 = story SylviasFateV1 Cards.sylviasFateV1

{- | Part one, read when the act draws this card from the Students' Fate deck:
"Reveal the <location>. Put this card aside, without reading the second part,
until a {reaction} trigger allows you to read the second part."

Part two is read by that location when it runs out of clues; it records the
student's fate and flips this card to whatever became of them.
-}
instance RunMessage SylviasFateV1 where
  runMessage msg s@(SylviasFateV1 attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      selectForMaybeM (locationIs Locations.impenetrableForest) reveal
      pure s
    _ -> SylviasFateV1 <$> liftRunMessage msg attrs
