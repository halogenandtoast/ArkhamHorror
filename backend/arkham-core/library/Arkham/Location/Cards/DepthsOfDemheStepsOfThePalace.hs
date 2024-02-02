module Arkham.Location.Cards.DepthsOfDemheStepsOfThePalace (
  depthsOfDemheStepsOfThePalace,
  DepthsOfDemheStepsOfThePalace (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher hiding (NonAttackDamageEffect)
import Arkham.Scenarios.DimCarcosa.Helpers
import Arkham.Story.Cards qualified as Story

newtype DepthsOfDemheStepsOfThePalace = DepthsOfDemheStepsOfThePalace LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData, HasAbilities)

depthsOfDemheStepsOfThePalace :: LocationCard DepthsOfDemheStepsOfThePalace
depthsOfDemheStepsOfThePalace =
  locationWith DepthsOfDemheStepsOfThePalace Cards.depthsOfDemheStepsOfThePalace 4 (PerPlayer 1)
    $ (canBeFlippedL .~ True)
    . (revealedL .~ True)

instance HasModifiersFor DepthsOfDemheStepsOfThePalace where
  getModifiersFor (InvestigatorTarget iid) (DepthsOfDemheStepsOfThePalace a) = do
    here <- iid `isAt` a
    pure $ toModifiers a [CannotPlay FastCard | here]
  getModifiersFor _ _ = pure []

instance RunMessage DepthsOfDemheStepsOfThePalace where
  runMessage msg (DepthsOfDemheStepsOfThePalace attrs) = case msg of
    Flip iid _ target | isTarget attrs target -> do
      readStory iid (toId attrs) Story.stepsOfThePalace
      pure . DepthsOfDemheStepsOfThePalace $ attrs & canBeFlippedL .~ False
    _ -> DepthsOfDemheStepsOfThePalace <$> runMessage msg attrs
