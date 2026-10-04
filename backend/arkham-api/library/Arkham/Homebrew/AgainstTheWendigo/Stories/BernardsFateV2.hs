module Arkham.Homebrew.AgainstTheWendigo.Stories.BernardsFateV2 (bernardsFateV2) where

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

newtype BernardsFateV2 = BernardsFateV2 StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | See 'Arkham.Homebrew.AgainstTheWendigo.Stories.BernardsFateV1'. In this
-- version Bernard did not come back at all; the card flips into the clearing
-- where he was killed.
bernardsFateV2 :: StoryCard BernardsFateV2
bernardsFateV2 = persistStory $ story BernardsFateV2 Cards.bernardsFateV2

instance HasAbilities BernardsFateV2 where
  getAbilities (BernardsFateV2 a) =
    [ restricted
        a
        1
        (notExists $ locationIs Locations.siteOfAncientStones <> LocationWithAnyClues)
        $ forced
        $ DiscoverClues #after Anyone (locationIs Locations.siteOfAncientStones) AnyValue
    ]

instance RunMessage BernardsFateV2 where
  runMessage msg s@(BernardsFateV2 attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      selectForMaybeM (locationIs Locations.siteOfAncientStones) reveal
      pure s
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      record YouHaveDiscoveredBernardsFate
      lead <- getLead
      drawCard lead =<< genCard Treacheries.theClearingOfTheSacrifices
      removeStory attrs
      pure s
    _ -> BernardsFateV2 <$> liftRunMessage msg attrs
