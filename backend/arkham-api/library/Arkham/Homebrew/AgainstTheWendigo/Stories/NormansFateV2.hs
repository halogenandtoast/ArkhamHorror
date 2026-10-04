module Arkham.Homebrew.AgainstTheWendigo.Stories.NormansFateV2 (normansFateV2) where

import Arkham.Ability
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Assets qualified as Assets
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Stories qualified as Cards
import Arkham.Homebrew.AgainstTheWendigo.Key
import Arkham.Matcher
import Arkham.Message.Lifted.Log (record)
import Arkham.Placement
import Arkham.Story.Import.Lifted hiding (DiscoverClues)

newtype NormansFateV2 = NormansFateV2 StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | See 'Arkham.Homebrew.AgainstTheWendigo.Stories.BernardsFateV1'. This is the
one version in which a student can be brought home: the card flips into Norman
himself, and the epilogue cares whether he is still alive at the end.
-}
normansFateV2 :: StoryCard NormansFateV2
normansFateV2 = persistStory $ story NormansFateV2 Cards.normansFateV2

instance HasAbilities NormansFateV2 where
  getAbilities (NormansFateV2 a) =
    [ restricted a 1 (notExists $ locationIs Locations.swamp <> LocationWithAnyClues)
        $ forced
        $ DiscoverClues #after Anyone (locationIs Locations.swamp) AnyValue
    ]

instance RunMessage NormansFateV2 where
  runMessage msg s@(NormansFateV2 attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      selectForMaybeM (locationIs Locations.swamp) reveal
      pure s
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      record YouHaveDiscoveredNormansFate
      -- "Revelation - An investigator in the Swamp takes control of Norman Falkner."
      selectForMaybeM (locationIs Locations.swamp) \lid ->
        createAssetAt_ Assets.normanFalkner (AtLocation lid)
      removeStory attrs
      pure s
    _ -> NormansFateV2 <$> liftRunMessage msg attrs
