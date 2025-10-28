module Arkham.Story.Cards.EndlessSecrets (endlessSecrets) where

import Arkham.ScenarioLogKey
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype EndlessSecrets = EndlessSecrets StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

endlessSecrets :: StoryCard EndlessSecrets
endlessSecrets = story EndlessSecrets Cards.endlessSecrets

instance RunMessage EndlessSecrets where
  runMessage msg s@(EndlessSecrets attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      push $ ScenarioCountIncrementBy SignOfTheGods 1
      pure s
    _ -> EndlessSecrets <$> liftRunMessage msg attrs
