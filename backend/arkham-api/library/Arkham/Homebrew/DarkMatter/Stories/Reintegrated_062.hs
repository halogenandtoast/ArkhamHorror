module Arkham.Homebrew.DarkMatter.Stories.Reintegrated_062 (reintegrated_062) where

import Arkham.GameValue
import Arkham.Homebrew.DarkMatter.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.DarkMatter.CardDefs.Stories qualified as Cards
import Arkham.Matcher
import Arkham.Story.Import.Lifted

newtype Reintegrated_062 = Reintegrated_062 StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

reintegrated_062 :: StoryCard Reintegrated_062
reintegrated_062 = story Reintegrated_062 Cards.reintegrated_062

instance RunMessage Reintegrated_062 where
  runMessage msg s@(Reintegrated_062 attrs) = runQueueT $ case msg of
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
    _ -> Reintegrated_062 <$> liftRunMessage msg attrs
