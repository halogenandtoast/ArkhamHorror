module Arkham.Location.Cards.SeaOfPitch_263 (seaOfPitch_263, SeaOfPitch_263 (..)) where

import Arkham.GameValue
import Arkham.Helpers.Log
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Story (readStory)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Prelude
import Arkham.ScenarioLogKey
import Arkham.Story.Cards qualified as Story

newtype SeaOfPitch_263 = SeaOfPitch_263 LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

seaOfPitch_263 :: LocationCard SeaOfPitch_263
seaOfPitch_263 = location SeaOfPitch_263 Cards.seaOfPitch_263 0 (PerPlayer 1)

instance HasModifiersFor SeaOfPitch_263 where
  getModifiersFor target (SeaOfPitch_263 attrs) | attrs `is` target = do
    n <- scenarioCount Distortion
    pure $ toModifiers attrs [ShroudModifier n | n > 0]
  getModifiersFor _ _ = pure []

instance HasAbilities SeaOfPitch_263 where
  getAbilities (SeaOfPitch_263 attrs) =
    veiled attrs []

instance RunMessage SeaOfPitch_263 where
  runMessage msg (SeaOfPitch_263 attrs) = case msg of
    Flip iid _ (isTarget attrs -> True) -> do
      readStory iid (toId attrs) Story.stillSurface
      pure . SeaOfPitch_263 $ attrs & canBeFlippedL .~ False
    _ -> SeaOfPitch_263 <$> runMessage msg attrs
