module Arkham.Story.Cards.GhastlyTunnels (GhastlyTunnels (..), ghastlyTunnels) where

import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted hiding (story)
import Arkham.Prelude
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Runner hiding (createEnemyAt, findEncounterCard)
import Arkham.Trait (Trait (Ghast))

newtype GhastlyTunnels = GhastlyTunnels StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ghastlyTunnels :: StoryCard GhastlyTunnels
ghastlyTunnels = story GhastlyTunnels Cards.ghastlyTunnels

instance RunMessage GhastlyTunnels where
  runMessage msg s@(GhastlyTunnels attrs) = runQueueT $ case msg of
    ResolveStory iid ResolveIt story' | story' == toId attrs -> do
      findEncounterCard @CardMatcher iid attrs $ #enemy <> withTrait Ghast
      pure s
    FoundEncounterCard _iid target card | isTarget attrs target -> do
      vaultsOfZin <- selectJust $ locationIs Locations.vaultsOfZin
      ghast <- createEnemyAt card vaultsOfZin
      placeClues attrs ghast 1
      pure s
    _ -> GhastlyTunnels <$> lift (runMessage msg attrs)
