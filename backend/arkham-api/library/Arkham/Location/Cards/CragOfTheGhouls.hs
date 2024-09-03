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
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Phase
import Arkham.Prelude
import Arkham.Story.Cards qualified as Story

newtype CragOfTheGhouls = CragOfTheGhouls LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cragOfTheGhouls :: LocationCard CragOfTheGhouls
cragOfTheGhouls = location CragOfTheGhouls Cards.cragOfTheGhouls 3 (PerPlayer 2)

instance HasModifiersFor CragOfTheGhouls where
  getModifiersFor (CardIdTarget cardId) (CragOfTheGhouls a) = do
    phase <- getPhase
    card <- getCard cardId
    if phase == MythosPhase && card `cardMatch` CardWithType TreacheryType
      then do
        history <- fmap fold . traverse (getHistory PhaseHistory) =<< select (investigatorAt a)
        pure $ toModifiers a [AddKeyword Keyword.Surge | length (historyTreacheriesDrawn history) == 1]
      else pure []
  getModifiersFor _ _ = pure []

instance HasAbilities CragOfTheGhouls where
  getAbilities (CragOfTheGhouls attrs) = veiled attrs []

instance RunMessage CragOfTheGhouls where
  runMessage msg (CragOfTheGhouls attrs) = case msg of
    Flip iid _ (isTarget attrs -> True) -> do
      readStory iid (toId attrs) Story.scoutingTheVale
      pure . CragOfTheGhouls $ attrs & canBeFlippedL .~ False
    _ -> CragOfTheGhouls <$> runMessage msg attrs
