module Arkham.Story.Cards.HastursEnd (
  HastursEnd (..),
  hastursEnd,
) where

import Arkham.Prelude

import Arkham.ScenarioLogKey
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Runner

newtype HastursEnd = HastursEnd StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hastursEnd :: StoryCard HastursEnd
hastursEnd = story HastursEnd Cards.hastursEnd

instance RunMessage HastursEnd where
  runMessage msg s@(HastursEnd attrs) = case msg of
    ResolveStory _ story' | story' == toId attrs -> do
      pushAll
        [ Remember KnowTheSecret
        , CheckDefeated (toSource attrs)
        ]
      pure s
    _ -> HastursEnd <$> runMessage msg attrs
