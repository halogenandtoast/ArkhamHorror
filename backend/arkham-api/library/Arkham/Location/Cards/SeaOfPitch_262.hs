module Arkham.Location.Cards.SeaOfPitch_262 (seaOfPitch_262, SeaOfPitch_262 (..)) where

import Arkham.GameValue
import Arkham.Helpers.Log
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Story (readStory)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Prelude
import Arkham.ScenarioLogKey
import Arkham.Story.Cards qualified as Story

newtype SeaOfPitch_262 = SeaOfPitch_262 LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

seaOfPitch_262 :: LocationCard SeaOfPitch_262
seaOfPitch_262 = location SeaOfPitch_262 Cards.seaOfPitch_262 0 (PerPlayer 1)

instance HasModifiersFor SeaOfPitch_262 where
  getModifiersFor target (SeaOfPitch_262 attrs) | attrs `is` target = do
    n <- scenarioCount Distortion
    pure $ toModifiers attrs [ShroudModifier n | n > 0]
  getModifiersFor _ _ = pure []

instance HasAbilities SeaOfPitch_262 where
  getAbilities (SeaOfPitch_262 attrs) =
    veiled attrs []

instance RunMessage SeaOfPitch_262 where
  runMessage msg (SeaOfPitch_262 attrs) = case msg of
    Flip iid _ (isTarget attrs -> True) -> do
      readStory iid (toId attrs) Story.spiderInfestedWaters
      pure . SeaOfPitch_262 $ attrs & canBeFlippedL .~ False
    _ -> SeaOfPitch_262 <$> runMessage msg attrs
