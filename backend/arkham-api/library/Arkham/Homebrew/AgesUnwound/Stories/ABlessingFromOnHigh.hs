module Arkham.Homebrew.AgesUnwound.Stories.ABlessingFromOnHigh (aBlessingFromOnHigh) where

import Arkham.Homebrew.AgesUnwound.CardDefs.Assets qualified as Assets
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.AgesUnwound.CardDefs.Stories qualified as Cards
import Arkham.Homebrew.AgesUnwound.CardDefs.Treacheries qualified as Treacheries
import Arkham.Homebrew.AgesUnwound.Missions.Helpers (completeTask, takeControlOfSetAsideRewardAsset)
import Arkham.Matcher
import Arkham.Message.Lifted.Move (moveTo)
import Arkham.Story.Import.Lifted

newtype ABlessingFromOnHigh = ABlessingFromOnHigh StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | The reverse of /Distant Entity/ (@:ages-unwound:144@).
aBlessingFromOnHigh :: StoryCard ABlessingFromOnHigh
aBlessingFromOnHigh = story ABlessingFromOnHigh Cards.aBlessingFromOnHigh

{- | "Move to Shanghai. Put the set-aside Wings of Damakairon asset into play
under your control. For the remainder of this scenario, it does not take up a
body slot. Complete Entreating the Gods. Remove this card from the game."

"This card" is Distant Entity, the other side of this one.
-}
instance RunMessage ABlessingFromOnHigh where
  runMessage msg s@(ABlessingFromOnHigh attrs) = runQueueT $ case msg of
    ResolveThisStory iid (is attrs -> True) -> do
      selectForMaybeM (locationIs Locations.shanghai) $ moveTo attrs iid
      takeControlOfSetAsideRewardAsset attrs iid Assets.wingsOfDamakairon #body
      completeTask Treacheries.entreatingTheGods
      for_ (storyOtherSide attrs) removeFromGame
      pure s
    _ -> ABlessingFromOnHigh <$> liftRunMessage msg attrs
