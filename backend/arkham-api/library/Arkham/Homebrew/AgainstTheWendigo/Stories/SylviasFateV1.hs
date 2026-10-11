module Arkham.Homebrew.AgainstTheWendigo.Stories.SylviasFateV1 (sylviasFateV1) where

import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Stories qualified as Cards
import Arkham.Homebrew.AgainstTheWendigo.Key
import Arkham.Matcher
import Arkham.Message.Lifted.Log (record)
import Arkham.Placement
import Arkham.Story.Import.Lifted

newtype SylviasFateV1 = SylviasFateV1 StoryAttrs
  deriving anyclass (IsStory, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | See 'Arkham.Homebrew.AgainstTheWendigo.Stories.BernardsFateV1'. Here the
forest itself took Sylvia; the card flips into The Heart of the Forest, whose
revelation drags everyone in the Impenetrable Forest into it.
-}
sylviasFateV1 :: StoryCard SylviasFateV1
sylviasFateV1 = persistStory $ story SylviasFateV1 Cards.sylviasFateV1

instance RunMessage SylviasFateV1 where
  runMessage msg s@(SylviasFateV1 attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) | attrs.placement /= Global -> do
      selectForMaybeM (locationIs Locations.impenetrableForest) reveal
      -- Put aside, but still on the table: the Impenetrable Forest's reaction reads
      -- the second part by resolving this card again.
      pure $ SylviasFateV1 $ attrs & placementL .~ Global
    -- The second part, asked for by the Impenetrable Forest once its clues run out.
    ResolveThisStory _ (is attrs -> True) -> do
      record YouHaveDiscoveredSylviasFate
      reveal =<< placeLocationCard Locations.theHeartOfTheForest
      removeStory attrs
      pure s
    _ -> SylviasFateV1 <$> liftRunMessage msg attrs
