module Arkham.Homebrew.DarkMatter.Stories.Reintegrated_063 (reintegrated_063) where

import Arkham.GameValue
import Arkham.Homebrew.DarkMatter.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.DarkMatter.CardDefs.Stories qualified as Cards
import Arkham.Matcher
import Arkham.Story.Import.Lifted

newtype Reintegrated_063 = Reintegrated_063 StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

reintegrated_063 :: StoryCard Reintegrated_063
reintegrated_063 = story Reintegrated_063 Cards.reintegrated_063

instance RunMessage Reintegrated_063 where
  runMessage msg s@(Reintegrated_063 attrs) = runQueueT $ case msg of
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
    _ -> Reintegrated_063 <$> liftRunMessage msg attrs
