module Arkham.Story.Cards.LangneauPerdu (
  LangneauPerdu (..),
  langneauPerdu,
) where

import Arkham.Prelude

import Arkham.ScenarioLogKey
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Runner

newtype LangneauPerdu = LangneauPerdu StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

langneauPerdu :: StoryCard LangneauPerdu
langneauPerdu = story LangneauPerdu Cards.langneauPerdu

instance RunMessage LangneauPerdu where
  runMessage msg s@(LangneauPerdu attrs) = case msg of
    ResolveStory _ _ story' | story' == toId attrs -> do
      push $ Remember InterviewedJordan
      pure s
    _ -> LangneauPerdu <$> runMessage msg attrs
