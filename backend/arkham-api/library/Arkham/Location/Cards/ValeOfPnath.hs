module Arkham.Location.Cards.ValeOfPnath (valeOfPnath, ValeOfPnath (..)) where

import Arkham.Game.Helpers (perPlayer)
import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Story (readStory)
import Arkham.Investigator.Types (Field (InvestigatorClues))
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Projection
import Arkham.Story.Cards qualified as Story

newtype ValeOfPnath = ValeOfPnath LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

valeOfPnath :: LocationCard ValeOfPnath
valeOfPnath = location ValeOfPnath Cards.valeOfPnath 4 (PerPlayer 1)

instance HasModifiersFor ValeOfPnath where
  getModifiersFor (InvestigatorTarget iid) (ValeOfPnath a) = do
    here <- iid `isAt` a
    hasClues <- fieldMap LocationClues (> 0) a.id
    pure $ toModifiers a $ guard (here && hasClues) *> [CannotPlay AnyCard, CannotCommitCards AnyCard]
  getModifiersFor _ _ = pure []

instance HasAbilities ValeOfPnath where
  getAbilities (ValeOfPnath attrs) = veiled attrs []

instance RunMessage ValeOfPnath where
  runMessage msg (ValeOfPnath attrs) = case msg of
    Flip iid _ (isTarget attrs -> True) -> do
      readStory iid (toId attrs) Story.theWayOut
      clues <- selectSum InvestigatorClues UneliminatedInvestigator
      n <- perPlayer 3
      pure . ValeOfPnath $ attrs & canBeFlippedL .~ (clues < n)
    _ -> ValeOfPnath <$> runMessage msg attrs
