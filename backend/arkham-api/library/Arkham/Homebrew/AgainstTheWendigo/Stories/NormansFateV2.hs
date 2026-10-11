module Arkham.Homebrew.AgainstTheWendigo.Stories.NormansFateV2 (normansFateV2) where

import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Assets qualified as Assets
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Stories qualified as Cards
import Arkham.Homebrew.AgainstTheWendigo.Key
import Arkham.Matcher
import Arkham.Message.Lifted.Log (record)
import Arkham.Placement
import Arkham.Story.Import.Lifted

newtype NormansFateV2 = NormansFateV2 StoryAttrs
  deriving anyclass (IsStory, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | See 'Arkham.Homebrew.AgainstTheWendigo.Stories.BernardsFateV1'. This is the
one version in which a student can be brought home: the card flips into Norman
himself, and the epilogue cares whether he is still alive at the end.
-}
normansFateV2 :: StoryCard NormansFateV2
normansFateV2 = persistStory $ story NormansFateV2 Cards.normansFateV2

instance RunMessage NormansFateV2 where
  runMessage msg s@(NormansFateV2 attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) | attrs.placement /= Global -> do
      selectForMaybeM (locationIs Locations.swamp) reveal
      -- Put aside, but still on the table: the Swamp's reaction reads
      -- the second part by resolving this card again.
      pure $ NormansFateV2 $ attrs & placementL .~ Global
    -- The second part, asked for by the Swamp once its clues run out.
    ResolveThisStory _ (is attrs -> True) -> do
      record YouHaveDiscoveredNormansFate
      -- "Revelation - An investigator in the Swamp takes control of Norman Falkner."
      selectForMaybeM (locationIs Locations.swamp) \lid ->
        createAssetAt_ Assets.normanFalkner (AtLocation lid)
      removeStory attrs
      pure s
    _ -> NormansFateV2 <$> liftRunMessage msg attrs
