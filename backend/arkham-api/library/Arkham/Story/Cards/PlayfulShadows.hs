module Arkham.Story.Cards.PlayfulShadows (playfulShadows) where

import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Log
import Arkham.ScenarioLogKey
import Arkham.Card
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype PlayfulShadows = PlayfulShadows StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

playfulShadows :: StoryCard PlayfulShadows
playfulShadows = story PlayfulShadows Cards.playfulShadows

instance RunMessage PlayfulShadows where
  runMessage msg s@(PlayfulShadows attrs) = runQueueT $ case msg of
    ResolveThisStory iid (is attrs -> True) -> do
      embarassedTheConsulate <- remembered EmbarrassedTheConsulate
      if embarassedTheConsulate
        then addToVictory iid attrs
        else do
          let enemy = lookupCard Enemies.uncannyShadowPlayfulShadows (toCardId attrs)
          removeStory attrs
          createEnemy_ enemy attrs.placement
      pure s
    _ -> PlayfulShadows <$> liftRunMessage msg attrs
