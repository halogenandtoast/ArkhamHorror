module Arkham.Story.Cards.TriumphAndSubjugation (triumphAndSubjugation) where

import Arkham.Helpers.FlavorText
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.BadBlood.Helpers
import Arkham.Scenarios.BadBlood.Meta
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype TriumphAndSubjugation = TriumphAndSubjugation StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

triumphAndSubjugation :: StoryCard TriumphAndSubjugation
triumphAndSubjugation = story TriumphAndSubjugation Cards.triumphAndSubjugation

instance RunMessage TriumphAndSubjugation where
  runMessage msg s@(TriumphAndSubjugation attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      whenJustM (selectOne agnesBaker) \agnes -> do
        n <- (.elspethMemories) <$> getBadBloodMeta
        clues <- perPlayer 2
        scenarioI18n $ chooseOrRunOneM agnes do
          when (n > 0) $ labeled' "collectMemoryFromElspeth" agnesStealsMemory
          countVar clues $ labeled' "gainCluesFromTokenBank" $ gainClues agnes (attrs.ability 1) clues
      pure s
    _ -> TriumphAndSubjugation <$> liftRunMessage msg attrs
