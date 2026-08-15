module Arkham.Homebrew.DarkMatter.Stories.Reintegrated_064 (reintegrated_064) where

import Arkham.GameValue
import Arkham.Homebrew.DarkMatter.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.DarkMatter.CardDefs.Stories qualified as Cards
import Arkham.Matcher
import Arkham.Story.Import.Lifted

newtype Reintegrated_064 = Reintegrated_064 StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

reintegrated_064 :: StoryCard Reintegrated_064
reintegrated_064 = story Reintegrated_064 Cards.reintegrated_064

instance RunMessage Reintegrated_064 where
  runMessage msg s@(Reintegrated_064 attrs) = runQueueT $ case msg of
    ResolveThisStory iid (is attrs -> True) -> do
      withMatch (enemyIs Enemies.theBOOGEYMAN) $ placeTokensOn attrs #horror 1
      doStep 1 msg
      addToVictory iid attrs
      for_ (storyOtherSide attrs) \case
        AssetTarget aid -> removeFromGame aid
        _ -> pure ()
      pure s
    DoStep 1 (ResolveThisStory iid (is attrs -> True)) -> do
      withMatch (enemyIs Enemies.theBOOGEYMAN <> EnemyWithTokens (Static 4) #horror) \boogeyman -> do
        defeatEnemy boogeyman iid attrs
      pure s
    _ -> Reintegrated_064 <$> liftRunMessage msg attrs
