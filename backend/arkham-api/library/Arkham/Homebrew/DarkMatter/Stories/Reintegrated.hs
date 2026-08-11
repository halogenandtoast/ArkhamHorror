module Arkham.Homebrew.DarkMatter.Stories.Reintegrated (
  reintegrated_062,
  reintegrated_063,
  reintegrated_064,
  reintegrated_065,
) where

import Arkham.Card.CardDef (CardDef)
import Arkham.Enemy.Types (Field (EnemyTokens))
import Arkham.Homebrew.DarkMatter.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.DarkMatter.CardDefs.Stories qualified as Cards
import Arkham.Id
import Arkham.Matcher
import Arkham.Projection
import Arkham.Story.Import.Lifted
import Arkham.Token qualified as Token

{- | All four Reintegrated cards (Alma, David, Tilde and Will) share one effect:

"Place 1 horror token on The Boogeyman from the token pool. If there are 4
horror tokens on The Boogeyman, it is defeated. Add this card to the victory
display. Victory 1."

The Boogeyman "cannot be attacked, damaged or evaded", so the horror tokens are
a bespoke counter rather than real damage — reaching 4 defeats it outright.
-}
resolveReintegrated :: ReverseQueue m => InvestigatorId -> StoryAttrs -> m ()
resolveReintegrated iid attrs = do
  selectOne (enemyIs Enemies.theBOOGEYMAN) >>= traverse_ \boogeyman -> do
    placeTokens attrs boogeyman Token.Horror 1
    horror <- fieldMap EnemyTokens (Token.countTokens Token.Horror) boogeyman
    when (horror >= 4) $ defeatEnemy boogeyman iid attrs
  addToVictory iid attrs

newtype Reintegrated = Reintegrated StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mkReintegrated :: CardDef -> StoryCard Reintegrated
mkReintegrated = story Reintegrated

reintegrated_062 :: StoryCard Reintegrated
reintegrated_062 = mkReintegrated Cards.reintegrated_062

reintegrated_063 :: StoryCard Reintegrated
reintegrated_063 = mkReintegrated Cards.reintegrated_063

reintegrated_064 :: StoryCard Reintegrated
reintegrated_064 = mkReintegrated Cards.reintegrated_064

reintegrated_065 :: StoryCard Reintegrated
reintegrated_065 = mkReintegrated Cards.reintegrated_065

instance RunMessage Reintegrated where
  runMessage msg s@(Reintegrated attrs) = runQueueT $ case msg of
    ResolveThisStory iid (is attrs -> True) -> do
      resolveReintegrated iid attrs
      pure s
    _ -> Reintegrated <$> liftRunMessage msg attrs
