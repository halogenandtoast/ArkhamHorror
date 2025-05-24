module Arkham.Story.Cards.TheFall (theFall) where

import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.DimCarcosa.Helpers
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype TheFall = TheFall StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theFall :: StoryCard TheFall
theFall = story TheFall Cards.theFall

instance RunMessage TheFall where
  runMessage msg s@(TheFall attrs) = runQueueT $ case msg of
    ResolveStory iid _ story' | story' == toId attrs -> do
      hastur <- selectJust $ EnemyWithTitle "Hastur"
      n <- perPlayer 2
      lid <- selectJust $ locationIs Locations.darkSpires
      chooseOneM iid $ scenarioI18n do
        labeled' "theFall.cannot" $ push $ SetFlippable lid False
        labeled' "theFall.push" do
          assignHorror iid attrs 2
          storyEnemyDamage iid n hastur
      pure s
    _ -> TheFall <$> liftRunMessage msg attrs
