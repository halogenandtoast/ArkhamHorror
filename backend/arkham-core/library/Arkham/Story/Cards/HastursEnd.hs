module Arkham.Story.Cards.HastursEnd (
  HastursEnd (..),
  hastursEnd,
) where

import Arkham.Prelude

import Arkham.Matcher
import Arkham.ScenarioLogKey
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Runner

newtype HastursEnd = HastursEnd StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

hastursEnd :: StoryCard HastursEnd
hastursEnd = story HastursEnd Cards.hastursEnd

instance RunMessage HastursEnd where
  runMessage msg s@(HastursEnd attrs) = case msg of
    ResolveStory _ _ story' | story' == toId attrs -> do
      mhastur <- selectOne $ EnemyWithTitle "Hastur"
      pushAll
        $ [Remember KnowTheSecret]
        <> [checkDefeated attrs hastur | hastur <- toList mhastur]
      pure s
    _ -> HastursEnd <$> runMessage msg attrs
