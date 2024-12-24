module Arkham.Story.Cards.MadnessInside (MadnessInside (..), madnessInside) where

import Arkham.Enemy.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Source
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted
import Arkham.Target
import Arkham.Trait (Trait (Possessed))
import Arkham.Window qualified as Window

newtype MadnessInside = MadnessInside StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

madnessInside :: StoryCard MadnessInside
madnessInside = story MadnessInside Cards.madnessInside

instance RunMessage MadnessInside where
  runMessage msg s@(MadnessInside attrs) = runQueueT $ case msg of
    ResolveStory iid ResolveIt story' | story' == toId attrs -> do
      possessed <- select $ EnemyWithTrait Possessed <> NonEliteEnemy
      chooseTargetM iid possessed $ handleTarget iid attrs
      addToVictory attrs
      pure s
    HandleTargetChoice iid (isSource attrs -> True) (EnemyTarget eid) -> do
      doom <- field EnemyDoom eid
      when (doom > 0) do
        push $ FlipDoom (toTarget eid) doom
        moveTokens attrs eid iid #clue doom
        checkAfter $ Window.TakeControlOfClues iid (toSource eid) doom
      toDiscardBy iid attrs eid
      pure s
    _ -> MadnessInside <$> liftRunMessage msg attrs
