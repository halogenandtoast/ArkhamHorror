module Arkham.Homebrew.AgesUnwound.Stories.TideOfPossibility (tideOfPossibility) where

import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Query (getLead)
import Arkham.Homebrew.AgesUnwound.CardDefs.Stories qualified as Cards
import Arkham.Story.Import.Lifted

newtype TideOfPossibility = TideOfPossibility StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | The reverse of the @:ages-unwound:197@ printing of /What Could Be/.
tideOfPossibility :: StoryCard TideOfPossibility
tideOfPossibility = story TideOfPossibility Cards.tideOfPossibility

{- | "The lead investigator draws the top 2[per_investigator] cards of the
encounter deck, one at a time. If they are defeated by a card drawn this way, the
new lead investigator draws the remainder of these cards. Remove What Could Be
from the game."

Each draw has to re-read who the lead is, so this is the two-part 'doStep'
countdown rather than a flat loop: a flat loop would bind the lead once and keep
drawing onto an eliminated investigator.
-}
instance RunMessage TideOfPossibility where
  runMessage msg s@(TideOfPossibility attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      n <- perPlayer 2
      doStep n msg
      for_ (storyOtherSide attrs) removeFromGame
      removeFromGame attrs
      pure s
    DoStep n (ResolveThisStory _ (is attrs -> True)) | n > 0 -> do
      lead <- getLead
      drawEncounterCard lead attrs
      doNextStep msg
      pure s
    _ -> TideOfPossibility <$> liftRunMessage msg attrs
