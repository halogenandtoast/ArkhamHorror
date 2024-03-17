module Arkham.Story.Cards.AStrangeGhoul (AStrangeGhoul (..), aStrangeGhoul) where

import Arkham.Game.Helpers (perPlayer)
import Arkham.Helpers.Act
import Arkham.Helpers.Query (getInvestigators)
import Arkham.Investigator.Types (Field (InvestigatorClues))
import Arkham.Matcher
import Arkham.Source
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype AStrangeGhoul = AStrangeGhoul StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aStrangeGhoul :: StoryCard AStrangeGhoul
aStrangeGhoul = story AStrangeGhoul Cards.aStrangeGhoul

instance RunMessage AStrangeGhoul where
  runMessage msg s@(AStrangeGhoul attrs) = runQueueT $ case msg of
    ResolveStory _ ResolveIt story' | story' == toId attrs -> do
      investigators <- getInvestigators
      clues <- selectSum InvestigatorClues UneliminatedInvestigator
      n <- perPlayer 3
      when (clues >= n) do
        actId <- getCurrentAct
        pushAll [SpendClues n investigators, AdvanceAct actId (toSource attrs) #other]
      pure s
    _ -> AStrangeGhoul <$> lift (runMessage msg attrs)
