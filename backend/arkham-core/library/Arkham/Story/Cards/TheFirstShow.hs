module Arkham.Story.Cards.TheFirstShow (
  TheFirstShow (..),
  theFirstShow,
) where

import Arkham.Prelude

import Arkham.ScenarioLogKey
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Runner

newtype TheFirstShow = TheFirstShow StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

theFirstShow :: StoryCard TheFirstShow
theFirstShow = story TheFirstShow Cards.theFirstShow

instance RunMessage TheFirstShow where
  runMessage msg s@(TheFirstShow attrs) = case msg of
    ResolveStory _ _ story' | story' == toId attrs -> do
      push $ Remember InterviewedSebastien
      pure s
    _ -> TheFirstShow <$> runMessage msg attrs
