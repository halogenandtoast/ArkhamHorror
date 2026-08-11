module Arkham.Homebrew.DarkMatter.Stories.WithoutATrace (withoutATrace) where

import Arkham.Helpers.Query (getSetAsideCard)
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.DarkMatter.CardDefs.Stories qualified as Cards
import Arkham.Homebrew.DarkMatter.Helpers (campaignI18n, returnToScanningDeck)
import Arkham.Location.Types (Field (LocationClues))
import Arkham.Matcher
import Arkham.Message (ReplaceStrategy (Swap))
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Story.Import.Lifted

newtype WithoutATrace = WithoutATrace StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

withoutATrace :: StoryCard WithoutATrace
withoutATrace = story WithoutATrace Cards.withoutATrace

{- | "You may either (choose one):
- Assume command of the ship: If there are no clues on Derelict Ship, replace the
  Derelict Ship with the set aside Cassilda location (keeping all tokens and
  attachments). Add this card to the victory display.
- Scavenge the ship for parts: Draw 1 card and gain 1 resource. Shuffle this card
  back into the scanning deck."
-}
instance RunMessage WithoutATrace where
  runMessage msg s@(WithoutATrace attrs) = runQueueT $ case msg of
    ResolveThisStory iid (is attrs -> True) -> do
      derelict <- selectOne $ locationIs Locations.derelictShip
      clueless <- maybe (pure False) (fieldMap LocationClues (== 0)) derelict
      chooseOneM iid $ campaignI18n do
        when clueless $ for_ derelict \lid ->
          labeled' "withoutATrace.assumeCommandOfTheShip" do
            cassilda <- getSetAsideCard Locations.theCassilda
            push $ ReplaceLocation lid cassilda Swap
            addToVictory iid attrs
        labeled' "withoutATrace.scavengeTheShipForParts" do
          drawCards iid attrs 1
          gainResources iid attrs 1
          returnToScanningDeck attrs
      pure s
    _ -> WithoutATrace <$> liftRunMessage msg attrs
