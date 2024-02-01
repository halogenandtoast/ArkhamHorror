module Arkham.Story.Cards.EndlessSecrets (EndlessSecrets (..), endlessSecrets) where

import Arkham.Prelude
import Arkham.ScenarioLogKey
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Runner

newtype EndlessSecrets = EndlessSecrets StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

endlessSecrets :: StoryCard EndlessSecrets
endlessSecrets = story EndlessSecrets Cards.endlessSecrets

instance RunMessage EndlessSecrets where
  runMessage msg s@(EndlessSecrets attrs) = case msg of
    ResolveStory _ ResolveIt story' | story' == toId attrs -> do
      push $ ScenarioCountIncrementBy SignOfTheGods 1
      pure s
    _ -> EndlessSecrets <$> runMessage msg attrs
