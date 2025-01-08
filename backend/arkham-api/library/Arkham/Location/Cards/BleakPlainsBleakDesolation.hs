module Arkham.Location.Cards.BleakPlainsBleakDesolation (bleakPlainsBleakDesolation) where

import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Location.Types (revealedL)
import Arkham.Matcher hiding (NonAttackDamageEffect)
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
  runMessage msg (BleakPlainsBleakDesolation attrs) = runQueueT $ case msg of
    Flip iid _ target | isTarget attrs target -> do
      readStory iid (toId attrs) Story.bleakDesolation
      pure . BleakPlainsBleakDesolation $ attrs & canBeFlippedL .~ False
    _ -> BleakPlainsBleakDesolation <$> liftRunMessage msg attrs
