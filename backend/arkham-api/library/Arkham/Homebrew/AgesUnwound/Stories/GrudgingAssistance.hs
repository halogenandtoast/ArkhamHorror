module Arkham.Homebrew.AgesUnwound.Stories.GrudgingAssistance (grudgingAssistance) where

import Arkham.Homebrew.AgesUnwound.CardDefs.Events qualified as Events
import Arkham.Homebrew.AgesUnwound.CardDefs.Stories qualified as Cards
import Arkham.Homebrew.AgesUnwound.CardDefs.Treacheries qualified as Treacheries
import Arkham.Homebrew.AgesUnwound.Missions.Helpers (addSetAsideCardToHandAsOwner, completeTask)
import Arkham.Story.Import.Lifted

newtype GrudgingAssistance = GrudgingAssistance StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | The reverse of /Exposition/ (@:ages-unwound:149@).
grudgingAssistance :: StoryCard GrudgingAssistance
grudgingAssistance = story GrudgingAssistance Cards.grudgingAssistance

{- | "Add the set-aside Agency Strike Team event to your hand. For the remainder
of this scenario, you are considered to own this card. Complete Higher Powers.
Remove this card from the game."
-}
instance RunMessage GrudgingAssistance where
  runMessage msg s@(GrudgingAssistance attrs) = runQueueT $ case msg of
    ResolveThisStory iid (is attrs -> True) -> do
      addSetAsideCardToHandAsOwner iid Events.agencyStrikeTeam
      completeTask Treacheries.higherPowers
      for_ (storyOtherSide attrs) removeFromGame
      pure s
    _ -> GrudgingAssistance <$> liftRunMessage msg attrs
