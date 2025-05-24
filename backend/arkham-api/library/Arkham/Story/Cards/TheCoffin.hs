module Arkham.Story.Cards.TheCoffin (theCoffin) where

import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Matcher
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype TheCoffin = TheCoffin StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theCoffin :: StoryCard TheCoffin
theCoffin = story TheCoffin Cards.theCoffin

instance RunMessage TheCoffin where
  runMessage msg s@(TheCoffin attrs) = runQueueT $ case msg of
    ResolveStory iid _ story' | story' == toId attrs -> do
      hastur <- selectJust $ EnemyWithTitle "Hastur"
      n <- perPlayer 1
      storyEnemyDamage iid n hastur
      pure s
    _ -> TheCoffin <$> liftRunMessage msg attrs
