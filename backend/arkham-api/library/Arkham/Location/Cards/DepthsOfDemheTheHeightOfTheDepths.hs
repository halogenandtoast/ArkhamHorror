module Arkham.Location.Cards.DepthsOfDemheTheHeightOfTheDepths (depthsOfDemheTheHeightOfTheDepths) where

import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Location.Types (revealedL)
import Arkham.Matcher hiding (NonAttackDamageEffect)
import Arkham.Scenarios.DimCarcosa.Helpers
import Arkham.Story.Cards qualified as Story

newtype DepthsOfDemheTheHeightOfTheDepths = DepthsOfDemheTheHeightOfTheDepths LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

instance HasModifiersFor DepthsOfDemheTheHeightOfTheDepths where
  getModifiersFor (DepthsOfDemheTheHeightOfTheDepths a) =
    modifySelect a (investigatorAt a) [CannotPlay FastCard]

depthsOfDemheTheHeightOfTheDepths
  :: LocationCard DepthsOfDemheTheHeightOfTheDepths
depthsOfDemheTheHeightOfTheDepths =
  locationWith
    DepthsOfDemheTheHeightOfTheDepths
    Cards.depthsOfDemheTheHeightOfTheDepths
    4
    (PerPlayer 1)
    $ (canBeFlippedL .~ True)
    . (revealedL .~ True)

instance RunMessage DepthsOfDemheTheHeightOfTheDepths where
  runMessage msg (DepthsOfDemheTheHeightOfTheDepths attrs) = runQueueT $ case msg of
    Flip iid _ (isTarget attrs -> True) -> do
      readStory iid (toId attrs) Story.theHeightOfTheDepths
      pure . DepthsOfDemheTheHeightOfTheDepths $ attrs & canBeFlippedL .~ False
    _ -> DepthsOfDemheTheHeightOfTheDepths <$> liftRunMessage msg attrs
