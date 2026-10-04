module Arkham.Homebrew.AgainstTheWendigo.Stories.BernardsFateV1 (bernardsFateV1) where

import Arkham.Ability
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Stories qualified as Cards
import Arkham.Homebrew.AgainstTheWendigo.Key
import Arkham.Matcher
import Arkham.Message.Lifted.Log (record)
import Arkham.Story.Import.Lifted hiding (DiscoverClues)

newtype BernardsFateV1 = BernardsFateV1 StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor)
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

instance HasAbilities BernardsFateV1 where
  getAbilities (BernardsFateV1 a) =
    [ restricted
        a
        1
        (notExists $ locationIs Locations.siteOfAncientStones <> LocationWithAnyClues)
        $ forced
        $ DiscoverClues #after Anyone (locationIs Locations.siteOfAncientStones) AnyValue
    ]

instance RunMessage BernardsFateV1 where
  runMessage msg s@(BernardsFateV1 attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      selectForMaybeM (locationIs Locations.siteOfAncientStones) reveal
      pure s
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      record YouHaveDiscoveredBernardsFate
      createEnemyAtLocationMatching_
        Enemies.bernardEpstein
        (locationIs Locations.siteOfAncientStones)
      removeStory attrs
      pure s
    _ -> BernardsFateV1 <$> liftRunMessage msg attrs
