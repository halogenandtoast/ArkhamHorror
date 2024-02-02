module Arkham.Story.Cards.BleakDesolation (
  BleakDesolation (..),
  bleakDesolation,
) where

import Arkham.Prelude

import Arkham.DamageEffect
import Arkham.Game.Helpers
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Runner

newtype BleakDesolation = BleakDesolation StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

bleakDesolation :: StoryCard BleakDesolation
bleakDesolation = story BleakDesolation Cards.bleakDesolation

instance RunMessage BleakDesolation where
  runMessage msg s@(BleakDesolation attrs) = case msg of
    ResolveStory iid _ story' | story' == toId attrs -> do
      hastur <- selectJust $ EnemyWithTitle "Hastur"
      n <- perPlayer 2
      push $ Msg.EnemyDamage hastur $ storyDamage iid n
      pure s
    _ -> BleakDesolation <$> runMessage msg attrs
