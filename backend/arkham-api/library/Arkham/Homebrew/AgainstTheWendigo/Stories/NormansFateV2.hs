module Arkham.Homebrew.AgainstTheWendigo.Stories.NormansFateV2 (normansFateV2) where

import Arkham.Helpers.Query (getLead)
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Assets qualified as Assets
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Stories qualified as Cards
import Arkham.Homebrew.AgainstTheWendigo.Key
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
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
      record NormanIsAlive
      -- The story is the back's own card, so it leaves before the back arrives.
      removeStory attrs
      selectForMaybeM (locationIs Locations.swamp) \swamp -> do
        norman <- createAssetAt Assets.normanFalkner (AtLocation swamp)
        {- "Revelation - An investigator in the Swamp takes control of Norman
        Falkner." An asset put into play by a story card resolves no Revelation
        -- it was never drawn -- so the take-control is done from here. With
        nobody in the Swamp he is left standing in it. -}
        investigators <- select $ InvestigatorAt (LocationWithId swamp)
        unless (null investigators) do
          lead <- getLead
          chooseOrRunOneM lead $ targets investigators (`takeControlOfAsset` norman)
      pure s
    _ -> NormansFateV2 <$> liftRunMessage msg attrs
