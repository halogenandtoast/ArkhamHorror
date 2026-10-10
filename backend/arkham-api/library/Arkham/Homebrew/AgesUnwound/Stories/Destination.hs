module Arkham.Homebrew.AgesUnwound.Stories.Destination (destination) where

import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.AgesUnwound.CardDefs.Stories qualified as Cards
import Arkham.Homebrew.AgesUnwound.CardDefs.Treacheries qualified as Treacheries
import Arkham.Homebrew.AgesUnwound.Missions.Helpers (completeTask)
import Arkham.Matcher
import Arkham.Message.Lifted.Move (moveTo)
import Arkham.Story.Import.Lifted

newtype Destination = Destination StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | The reverse of /Another Realm/ (@:ages-unwound:142@).
destination :: StoryCard Destination
destination = story Destination Cards.destination

{- | "In your Campaign Log, record that /you took tea with the ruler of a strange
dimension./ Complete Strange Portal. Each investigator at this location moves to
Paris. Flip this card over and add it to the victory display."
-}
instance RunMessage Destination where
  runMessage msg s@(Destination attrs) = runQueueT $ case msg of
    ResolveThisStory iid (is attrs -> True) -> do
      -- TODO(ages-unwound): no campaign-log key exists for "you took tea with
      -- the ruler of a strange dimension" -- it is absent from Key.hs and from
      -- the structure doc's Scenario V list, and Key.hs is the orchestrator's.
      -- Nothing in Scenario V's resolution reads it, so the record is the only
      -- part of this card still missing.
      completeTask Treacheries.strangePortal
      for_ (storyOtherSide attrs >>= (.location)) \lid -> do
        selectForMaybeM (locationIs Locations.paris) \paris ->
          selectEach (InvestigatorAt $ LocationWithId lid) \iid' -> moveTo attrs iid' paris
        addToVictory iid lid
      pure s
    _ -> Destination <$> liftRunMessage msg attrs
