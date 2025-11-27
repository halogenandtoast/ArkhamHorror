module Arkham.Location.Cards.CragOfTheGhouls (cragOfTheGhouls, CragOfTheGhouls (..)) where

import Arkham.Card
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Story (readStory)
import Arkham.History (History (historyTreacheriesDrawn))
import Arkham.History.Types
import Arkham.Keyword qualified as Keyword
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Phase
import Arkham.Story.Cards qualified as Story

newtype CragOfTheGhouls = CragOfTheGhouls LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cragOfTheGhouls :: LocationCard CragOfTheGhouls
cragOfTheGhouls = location CragOfTheGhouls Cards.cragOfTheGhouls 3 (PerPlayer 2)

instance HasModifiersFor CragOfTheGhouls where
  getModifiersFor (CragOfTheGhouls a) = do
    phase <- getPhase
    when (phase == MythosPhase) do
      history <- fmap fold . traverse (getHistory PhaseHistory) =<< select (investigatorAt a)
      when (length (historyTreacheriesDrawn history) == 1) do
        cards <- findAllCards (`cardMatch` CardWithType TreacheryType)
        modifyEach a (map (CardIdTarget . toCardId) cards) [AddKeyword Keyword.Surge]

instance HasAbilities CragOfTheGhouls where
  getAbilities (CragOfTheGhouls attrs) = veiled attrs []

instance RunMessage CragOfTheGhouls where
  runMessage msg (CragOfTheGhouls attrs) = runQueueT $ case msg of
    Flip iid _ (isTarget attrs -> True) -> do
      readStory iid (toId attrs) Story.scoutingTheVale
      pure . CragOfTheGhouls $ attrs & canBeFlippedL .~ False
    _ -> CragOfTheGhouls <$> liftRunMessage msg attrs
