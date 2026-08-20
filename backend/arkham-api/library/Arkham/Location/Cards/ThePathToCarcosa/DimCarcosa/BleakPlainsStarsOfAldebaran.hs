module Arkham.Location.Cards.ThePathToCarcosa.DimCarcosa.BleakPlainsStarsOfAldebaran (bleakPlainsStarsOfAldebaran) where

import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Location.CardDefs.ThePathToCarcosa.DimCarcosa qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Location.Types (revealedL)
import Arkham.Matcher hiding (NonAttackDamageEffect)
import Arkham.Scenarios.ThePathToCarcosa.DimCarcosa.Helpers
import Arkham.Story.CardDefs.ThePathToCarcosa.DimCarcosa qualified as Story

newtype BleakPlainsStarsOfAldebaran = BleakPlainsStarsOfAldebaran LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

bleakPlainsStarsOfAldebaran :: LocationCard BleakPlainsStarsOfAldebaran
bleakPlainsStarsOfAldebaran =
  locationWith BleakPlainsStarsOfAldebaran Cards.bleakPlainsStarsOfAldebaran 4 (PerPlayer 1)
    $ (canBeFlippedL .~ True)
    . (revealedL .~ True)

instance HasModifiersFor BleakPlainsStarsOfAldebaran where
  getModifiersFor (BleakPlainsStarsOfAldebaran a) = do
    modifySelect a (investigatorAt a) [CannotPlay IsAlly]

instance RunMessage BleakPlainsStarsOfAldebaran where
  runMessage msg (BleakPlainsStarsOfAldebaran attrs) = runQueueT $ case msg of
    Flip iid _ (isTarget attrs -> True) -> do
      readStory iid (toId attrs) Story.starsOfAldebaran
      pure . BleakPlainsStarsOfAldebaran $ attrs & canBeFlippedL .~ False
    _ -> BleakPlainsStarsOfAldebaran <$> liftRunMessage msg attrs
