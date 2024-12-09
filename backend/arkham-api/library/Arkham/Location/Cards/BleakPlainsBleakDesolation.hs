module Arkham.Location.Cards.BleakPlainsBleakDesolation (
  bleakPlainsBleakDesolation,
  BleakPlainsBleakDesolation (..),
) where

import Arkham.Classes
import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher hiding (NonAttackDamageEffect)
import Arkham.Prelude
import Arkham.Scenarios.DimCarcosa.Helpers
import Arkham.Story.Cards qualified as Story

newtype BleakPlainsBleakDesolation = BleakPlainsBleakDesolation LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

bleakPlainsBleakDesolation :: LocationCard BleakPlainsBleakDesolation
bleakPlainsBleakDesolation =
  locationWith BleakPlainsBleakDesolation Cards.bleakPlainsBleakDesolation 4 (PerPlayer 1)
    $ (canBeFlippedL .~ True)
    . (revealedL .~ True)

instance HasModifiersFor BleakPlainsBleakDesolation where
  getModifiersFor (BleakPlainsBleakDesolation a) = do
    modifySelect a (investigatorAt a) [CannotPlay IsAlly]

instance RunMessage BleakPlainsBleakDesolation where
  runMessage msg (BleakPlainsBleakDesolation attrs) = case msg of
    Flip iid _ target | isTarget attrs target -> do
      readStory iid (toId attrs) Story.bleakDesolation
      pure . BleakPlainsBleakDesolation $ attrs & canBeFlippedL .~ False
    _ -> BleakPlainsBleakDesolation <$> runMessage msg attrs
