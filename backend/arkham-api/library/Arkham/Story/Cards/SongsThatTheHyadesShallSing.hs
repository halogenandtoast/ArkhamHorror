module Arkham.Story.Cards.SongsThatTheHyadesShallSing (songsThatTheHyadesShallSing) where

import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Matcher hiding (NonAttackDamageEffect)
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype SongsThatTheHyadesShallSing = SongsThatTheHyadesShallSing StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

songsThatTheHyadesShallSing :: StoryCard SongsThatTheHyadesShallSing
songsThatTheHyadesShallSing = story SongsThatTheHyadesShallSing Cards.songsThatTheHyadesShallSing

instance RunMessage SongsThatTheHyadesShallSing where
  runMessage msg s@(SongsThatTheHyadesShallSing attrs) = runQueueT $ case msg of
    ResolveStory iid _ story' | story' == toId attrs -> do
      hastur <- selectJust $ EnemyWithTitle "Hastur"
      n <- perPlayer 1
      storyEnemyDamage iid n hastur
      exhaustThis hastur
      selectEach (investigatorEngagedWith hastur) (`disengageEnemy` hastur)
      pure s
    _ -> SongsThatTheHyadesShallSing <$> liftRunMessage msg attrs
