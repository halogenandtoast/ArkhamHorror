module Arkham.Story.Cards.TheCoffin (
  TheCoffin (..),
  theCoffin,
) where

import Arkham.Prelude

import Arkham.DamageEffect
import Arkham.Game.Helpers
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Runner

newtype TheCoffin = TheCoffin StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theCoffin :: StoryCard TheCoffin
theCoffin = story TheCoffin Cards.theCoffin

instance RunMessage TheCoffin where
  runMessage msg s@(TheCoffin attrs) = case msg of
    ResolveStory iid _ story' | story' == toId attrs -> do
      hastur <- selectJust $ EnemyWithTitle "Hastur"
      n <- perPlayer 1
      push $ Msg.EnemyDamage hastur $ storyDamage iid n
      pure s
    _ -> TheCoffin <$> runMessage msg attrs
