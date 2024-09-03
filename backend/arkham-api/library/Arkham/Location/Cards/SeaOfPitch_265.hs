module Arkham.Location.Cards.SeaOfPitch_265 (seaOfPitch_265, SeaOfPitch_265 (..)) where

import Arkham.Game.Helpers (perPlayer)
import Arkham.GameValue
import Arkham.Helpers.Log
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Story (readStory)
import Arkham.Investigator.Types (Field (InvestigatorClues))
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Prelude
import Arkham.ScenarioLogKey
import Arkham.Story.Cards qualified as Story

newtype SeaOfPitch_265 = SeaOfPitch_265 LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

seaOfPitch_265 :: LocationCard SeaOfPitch_265
seaOfPitch_265 = location SeaOfPitch_265 Cards.seaOfPitch_265 0 (PerPlayer 1)

instance HasModifiersFor SeaOfPitch_265 where
  getModifiersFor target (SeaOfPitch_265 attrs) | attrs `is` target = do
    n <- scenarioCount Distortion
    pure $ toModifiers attrs [ShroudModifier n | n > 0]
  getModifiersFor _ _ = pure []

instance HasAbilities SeaOfPitch_265 where
  getAbilities (SeaOfPitch_265 attrs) =
    veiled attrs []

instance RunMessage SeaOfPitch_265 where
  runMessage msg (SeaOfPitch_265 attrs) = case msg of
    Flip iid _ (isTarget attrs -> True) -> do
      readStory iid (toId attrs) Story.centerOfTheSea
      clues <- selectSum InvestigatorClues UneliminatedInvestigator
      n <- perPlayer 3
      pure . SeaOfPitch_265 $ attrs & canBeFlippedL .~ (clues < n)
    _ -> SeaOfPitch_265 <$> runMessage msg attrs
