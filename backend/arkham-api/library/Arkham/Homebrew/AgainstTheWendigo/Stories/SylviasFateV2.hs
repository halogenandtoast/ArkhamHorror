module Arkham.Homebrew.AgainstTheWendigo.Stories.SylviasFateV2 (sylviasFateV2) where

import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Stories qualified as Cards
import Arkham.Homebrew.AgainstTheWendigo.Key
import Arkham.Matcher
import Arkham.Message.Lifted.Log (record)
import Arkham.Placement
import Arkham.Story.Import.Lifted

newtype SylviasFateV2 = SylviasFateV2 StoryAttrs
  deriving anyclass (IsStory, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | See 'Arkham.Homebrew.AgainstTheWendigo.Stories.BernardsFateV1'. Here Sylvia
came back as something that walks; the card flips into her.
-}
sylviasFateV2 :: StoryCard SylviasFateV2
sylviasFateV2 = persistStory $ story SylviasFateV2 Cards.sylviasFateV2

instance RunMessage SylviasFateV2 where
  runMessage msg s@(SylviasFateV2 attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) | attrs.placement /= Global -> do
      selectForMaybeM (locationIs Locations.impenetrableForest) reveal
      -- Put aside, but still on the table: the Impenetrable Forest's reaction reads
      -- the second part by resolving this card again.
      pure $ SylviasFateV2 $ attrs & placementL .~ Global
    -- The second part, asked for by the Impenetrable Forest once its clues run out.
    ResolveThisStory _ (is attrs -> True) -> do
      record YouHaveDiscoveredSylviasFate
      -- The story is the back's own card, so it leaves before the back arrives.
      removeStory attrs
      createEnemyAtLocationMatching_
        Enemies.sylviaDavidson
        (locationIs Locations.impenetrableForest)
      pure s
    _ -> SylviasFateV2 <$> liftRunMessage msg attrs
