module Arkham.Story.Cards.TracesToNowhere.UnlockingTheTruth (unlockingTheTruth) where

import Arkham.Card
import Arkham.Helpers.Location (swapLocation)
import Arkham.Location.CardDefs.TracesToNowhere qualified as Locations
import Arkham.Matcher
import Arkham.Story.CardDefs.TracesToNowhere qualified as Cards
import Arkham.Story.Import.Lifted

newtype UnlockingTheTruth = UnlockingTheTruth StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unlockingTheTruth :: StoryCard UnlockingTheTruth
unlockingTheTruth = story UnlockingTheTruth Cards.unlockingTheTruth

instance RunMessage UnlockingTheTruth where
  runMessage msg s@(UnlockingTheTruth attrs) = runQueueT $ case msg of
    {- "Flip this card over and swap it with the set-aside Meiger Estate (Beyond
    the Black Door), keeping all tokens and attachments." -}
    ResolveThisStory _ (is attrs -> True) -> do
      selectForMaybeM (locationIs Locations.meigerEstateUnderNewOwnership) \lid ->
        swapLocation lid =<< genCard Locations.meigerEstateBeyondTheBlackDoor
      pure s
    _ -> UnlockingTheTruth <$> liftRunMessage msg attrs
