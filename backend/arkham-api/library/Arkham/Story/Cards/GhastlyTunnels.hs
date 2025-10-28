module Arkham.Story.Cards.GhastlyTunnels (ghastlyTunnels) where

import Arkham.Enemy.Creation (createExhausted)
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Target
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted
import Arkham.Trait (Trait (Ghast))

newtype GhastlyTunnels = GhastlyTunnels StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ghastlyTunnels :: StoryCard GhastlyTunnels
ghastlyTunnels = story GhastlyTunnels Cards.ghastlyTunnels

instance RunMessage GhastlyTunnels where
  runMessage msg s@(GhastlyTunnels attrs) = runQueueT $ case msg of
    ResolveThisStory iid (is attrs -> True) -> do
      findEncounterCard @CardMatcher iid attrs $ #enemy <> withTrait Ghast
      pure s
    FoundEncounterCard _iid (isTarget attrs -> True) card -> do
      vaultsOfZin <- selectJust $ locationIs Locations.vaultsOfZin
      ghast <- createEnemyAtEdit card vaultsOfZin createExhausted
      placeClues attrs ghast 1
      pure s
    _ -> GhastlyTunnels <$> liftRunMessage msg attrs
