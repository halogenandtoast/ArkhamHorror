module Arkham.Story.Cards.TimorousShadows (timorousShadows) where

import Arkham.Card
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Log
import Arkham.ScenarioLogKey
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype TimorousShadows = TimorousShadows StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

timorousShadows :: StoryCard TimorousShadows
timorousShadows = story TimorousShadows Cards.timorousShadows

instance RunMessage TimorousShadows where
  runMessage msg s@(TimorousShadows attrs) = runQueueT $ case msg of
    ResolveThisStory iid (is attrs -> True) -> do
      peeredBeyond <- remembered PeeredBeyond
      if peeredBeyond
        then addToVictory iid attrs
        else do
          let enemy = lookupCard Enemies.uncannyShadowTimorousShadows (toCardId attrs)
          removeStory attrs
          createEnemy_ enemy attrs.placement
      pure s
    _ -> TimorousShadows <$> liftRunMessage msg attrs
