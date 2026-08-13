module Arkham.Homebrew.DarkMatter.Stories.Reintegrated_064 (reintegrated_064) where

import Arkham.Enemy.Types (Field (EnemyTokens))
import Arkham.Homebrew.DarkMatter.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.DarkMatter.CardDefs.Stories qualified as Cards
import Arkham.Matcher
import Arkham.Projection
import Arkham.Story.Import.Lifted
import Arkham.Token qualified as Token

{- | The back of one of Public School 187's four children:

"Place 1 horror token on The Boogeyman from the token pool. If there are 4 horror
tokens on The Boogeyman, it is defeated. Add this card to the victory display.
Victory 1."

The Boogeyman "cannot be attacked, damaged or evaded", so the horror tokens are a
bespoke counter rather than real damage — reaching 4 defeats it outright.
-}
newtype Reintegrated_064 = Reintegrated_064 StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

reintegrated_064 :: StoryCard Reintegrated_064
reintegrated_064 = story Reintegrated_064 Cards.reintegrated_064

instance RunMessage Reintegrated_064 where
  runMessage msg s@(Reintegrated_064 attrs) = runQueueT $ case msg of
    ResolveThisStory iid (is attrs -> True) -> do
      selectOne (enemyIs Enemies.theBOOGEYMAN) >>= traverse_ \boogeyman -> do
        placeTokens attrs boogeyman Token.Horror 1
        horror <- fieldMap EnemyTokens (Token.countTokens Token.Horror) boogeyman
        when (horror >= 4) $ defeatEnemy boogeyman iid attrs
      addToVictory iid attrs
      pure s
    _ -> Reintegrated_064 <$> liftRunMessage msg attrs
