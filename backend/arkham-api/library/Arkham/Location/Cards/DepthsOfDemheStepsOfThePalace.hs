module Arkham.Location.Cards.DepthsOfDemheStepsOfThePalace (depthsOfDemheStepsOfThePalace) where

import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Location.Types (revealedL)
import Arkham.Matcher hiding (NonAttackDamageEffect)
import Arkham.Scenarios.DimCarcosa.Helpers
import Arkham.Story.Cards qualified as Story

newtype DepthsOfDemheStepsOfThePalace = DepthsOfDemheStepsOfThePalace LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

depthsOfDemheStepsOfThePalace :: LocationCard DepthsOfDemheStepsOfThePalace
depthsOfDemheStepsOfThePalace =
  locationWith DepthsOfDemheStepsOfThePalace Cards.depthsOfDemheStepsOfThePalace 4 (PerPlayer 1)
    $ (canBeFlippedL .~ True)
    . (revealedL .~ True)

instance HasModifiersFor DepthsOfDemheStepsOfThePalace where
  getModifiersFor (DepthsOfDemheStepsOfThePalace a) =
    modifySelect a (investigatorAt a) [CannotPlay FastCard]

instance RunMessage DepthsOfDemheStepsOfThePalace where
  runMessage msg (DepthsOfDemheStepsOfThePalace attrs) = runQueueT $ case msg of
    Flip iid _ target | isTarget attrs target -> do
      readStory iid (toId attrs) Story.stepsOfThePalace
      pure . DepthsOfDemheStepsOfThePalace $ attrs & canBeFlippedL .~ False
    _ -> DepthsOfDemheStepsOfThePalace <$> liftRunMessage msg attrs
