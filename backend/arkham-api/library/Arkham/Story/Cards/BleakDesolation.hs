module Arkham.Story.Cards.BleakDesolation (bleakDesolation) where

import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Matcher
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype BleakDesolation = BleakDesolation StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bleakDesolation :: StoryCard BleakDesolation
bleakDesolation = story BleakDesolation Cards.bleakDesolation

instance RunMessage BleakDesolation where
  runMessage msg s@(BleakDesolation attrs) = runQueueT $ case msg of
    ResolveStory iid _ story' | story' == toId attrs -> do
      hastur <- selectJust $ EnemyWithTitle "Hastur"
      n <- perPlayer 2
      storyEnemyDamage iid n hastur
      pure s
    _ -> BleakDesolation <$> liftRunMessage msg attrs
