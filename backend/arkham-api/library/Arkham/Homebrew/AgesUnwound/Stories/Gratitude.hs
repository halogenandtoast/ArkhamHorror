module Arkham.Homebrew.AgesUnwound.Stories.Gratitude (gratitude) where

import Arkham.Homebrew.AgesUnwound.CardDefs.Skills qualified as Skills
import Arkham.Homebrew.AgesUnwound.CardDefs.Stories qualified as Cards
import Arkham.Homebrew.AgesUnwound.CardDefs.Treacheries qualified as Treacheries
import Arkham.Homebrew.AgesUnwound.Missions.Helpers (addSetAsideCardToHandAsOwner, completeTask)
import Arkham.Story.Import.Lifted

newtype Gratitude = Gratitude StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | The reverse of /Savage Yeti/ (@:ages-unwound:147@).
gratitude :: StoryCard Gratitude
gratitude = story Gratitude Cards.gratitude

{- | "Add the set-aside Monastic Training skill to your hand. For the remainder
of this scenario, you are considered to own this card. Complete Enemy of My
Enemy. Remove this card from the game."
-}
instance RunMessage Gratitude where
  runMessage msg s@(Gratitude attrs) = runQueueT $ case msg of
    ResolveThisStory iid (is attrs -> True) -> do
      addSetAsideCardToHandAsOwner iid Skills.monasticTraining
      completeTask Treacheries.enemyOfMyEnemy
      for_ (storyOtherSide attrs) removeFromGame
      pure s
    _ -> Gratitude <$> liftRunMessage msg attrs
