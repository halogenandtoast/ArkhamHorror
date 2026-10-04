module Arkham.Homebrew.AgainstTheWendigo.Stories.NormansFateV1 (normansFateV1) where

import Arkham.Ability
import Arkham.Card (genCard)
import Arkham.Helpers.Query (getLead)
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Stories qualified as Cards
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Treacheries qualified as Treacheries
import Arkham.Homebrew.AgainstTheWendigo.Key
import Arkham.Matcher
import Arkham.Message.Lifted.Log (record)
import Arkham.Story.Import.Lifted hiding (DiscoverClues)

newtype NormansFateV1 = NormansFateV1 StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | See 'Arkham.Homebrew.AgainstTheWendigo.Stories.BernardsFateV1'. In this
-- version the swamp ate Norman; the card flips into what is still in there.
normansFateV1 :: StoryCard NormansFateV1
normansFateV1 = persistStory $ story NormansFateV1 Cards.normansFateV1

instance HasAbilities NormansFateV1 where
  getAbilities (NormansFateV1 a) =
    [ restricted a 1 (notExists $ locationIs Locations.swamp <> LocationWithAnyClues)
        $ forced
        $ DiscoverClues #after Anyone (locationIs Locations.swamp) AnyValue
    ]

instance RunMessage NormansFateV1 where
  runMessage msg s@(NormansFateV1 attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      selectForMaybeM (locationIs Locations.swamp) reveal
      pure s
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      record YouHaveDiscoveredNormansFate
      lead <- getLead
      drawCard lead =<< genCard Treacheries.manEaters
      removeStory attrs
      pure s
    _ -> NormansFateV1 <$> liftRunMessage msg attrs
