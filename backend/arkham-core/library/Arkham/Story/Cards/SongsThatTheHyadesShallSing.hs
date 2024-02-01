module Arkham.Story.Cards.SongsThatTheHyadesShallSing (
  SongsThatTheHyadesShallSing (..),
  songsThatTheHyadesShallSing,
) where

import Arkham.Prelude

import Arkham.DamageEffect
import Arkham.Game.Helpers
import Arkham.Matcher hiding (NonAttackDamageEffect)
import Arkham.Message qualified as Msg
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Runner

newtype SongsThatTheHyadesShallSing = SongsThatTheHyadesShallSing StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

songsThatTheHyadesShallSing :: StoryCard SongsThatTheHyadesShallSing
songsThatTheHyadesShallSing = story SongsThatTheHyadesShallSing Cards.songsThatTheHyadesShallSing

instance RunMessage SongsThatTheHyadesShallSing where
  runMessage msg s@(SongsThatTheHyadesShallSing attrs) = case msg of
    ResolveStory iid _ story' | story' == toId attrs -> do
      hastur <- selectJust $ EnemyWithTitle "Hastur"
      investigatorIds <- selectList $ investigatorEngagedWith hastur
      n <- perPlayer 1
      pushAll
        $ [ Msg.EnemyDamage hastur $ storyDamage iid n
          , Exhaust (EnemyTarget hastur)
          ]
        <> [DisengageEnemy iid' hastur | iid' <- investigatorIds]
      pure s
    _ -> SongsThatTheHyadesShallSing <$> runMessage msg attrs
