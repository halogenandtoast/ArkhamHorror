module Arkham.Story.Cards.MappingTheStreets (mappingTheStreets) where

import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Matcher
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype MappingTheStreets = MappingTheStreets StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mappingTheStreets :: StoryCard MappingTheStreets
mappingTheStreets = story MappingTheStreets Cards.mappingTheStreets

instance RunMessage MappingTheStreets where
  runMessage msg s@(MappingTheStreets attrs) = runQueueT $ case msg of
    ResolveStory iid _ story' | story' == toId attrs -> do
      hastur <- selectJust $ EnemyWithTitle "Hastur"
      n <- perPlayer 1
      sid <- getRandom
      beginSkillTest sid iid attrs iid #intellect (Fixed 3)
      storyEnemyDamage iid n hastur
      pure s
    FailedThisSkillTestBy iid (isSource attrs -> True) n -> do
      assignHorror iid attrs n
      pure s
    _ -> MappingTheStreets <$> liftRunMessage msg attrs
