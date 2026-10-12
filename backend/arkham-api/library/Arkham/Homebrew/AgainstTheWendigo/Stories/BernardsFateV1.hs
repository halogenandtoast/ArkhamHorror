module Arkham.Homebrew.AgainstTheWendigo.Stories.BernardsFateV1 (bernardsFateV1) where

import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Stories qualified as Cards
import Arkham.Homebrew.AgainstTheWendigo.Key
import Arkham.Matcher
import Arkham.Message.Lifted.Log (record)
import Arkham.Placement
import Arkham.Story.Import.Lifted

newtype BernardsFateV1 = BernardsFateV1 StoryAttrs
  deriving anyclass (IsStory, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | Part one is read when an act draws this card from the Students' Fate deck:
"Reveal the Site of Ancient Stones. Put Bernard's Fate aside, without reading
the second part, until a {reaction} trigger allows you to read the second part."

The trigger is that location running out of clues, so the card persists past its
first resolution and watches for it. Part two records the fate and flips the
card -- in this version, into Bernard himself.
-}
bernardsFateV1 :: StoryCard BernardsFateV1
bernardsFateV1 = persistStory $ story BernardsFateV1 Cards.bernardsFateV1

instance RunMessage BernardsFateV1 where
  runMessage msg s@(BernardsFateV1 attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) | attrs.placement /= Global -> do
      selectForMaybeM (locationIs Locations.siteOfAncientStones) reveal
      -- Put aside, but still on the table: the Site of Ancient Stones's reaction reads
      -- the second part by resolving this card again.
      pure $ BernardsFateV1 $ attrs & placementL .~ Global
    -- The second part, asked for by the Site of Ancient Stones once its clues run out.
    ResolveThisStory _ (is attrs -> True) -> do
      record YouHaveDiscoveredBernardsFate
      -- The story is the back's own card, so it leaves before the back arrives.
      removeStory attrs
      createEnemyAtLocationMatching_
        Enemies.bernardEpstein
        (locationIs Locations.siteOfAncientStones)
      pure s
    _ -> BernardsFateV1 <$> liftRunMessage msg attrs
