module Arkham.Story.Cards.LagneauPerdu (
  LagneauPerdu (..),
  lagneauPerdu,
) where

import Arkham.Prelude

import Arkham.ScenarioLogKey
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Runner

newtype LagneauPerdu = LagneauPerdu StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

lagneauPerdu :: StoryCard LagneauPerdu
lagneauPerdu = story LagneauPerdu Cards.lagneauPerdu

instance RunMessage LagneauPerdu where
  runMessage msg s@(LagneauPerdu attrs) = case msg of
    ResolveStory _ _ story' | story' == toId attrs -> do
      push $ Remember InterviewedJordan
      pure s
    _ -> LagneauPerdu <$> runMessage msg attrs
