module Arkham.Homebrew.DarkMatter.Stories.Reintegrated_065 (reintegrated_065) where

import Arkham.GameValue
import Arkham.Homebrew.DarkMatter.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.DarkMatter.CardDefs.Stories qualified as Cards
import Arkham.Matcher
import Arkham.Story.Import.Lifted

newtype Reintegrated_065 = Reintegrated_065 StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

reintegrated_065 :: StoryCard Reintegrated_065
reintegrated_065 = story Reintegrated_065 Cards.reintegrated_065

instance RunMessage Reintegrated_065 where
  runMessage msg s@(Reintegrated_065 attrs) = runQueueT $ case msg of
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
    _ -> Reintegrated_065 <$> liftRunMessage msg attrs
