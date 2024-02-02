module Arkham.Location.Cards.DepthsOfDemheTheHeightOfTheDepths (
  depthsOfDemheTheHeightOfTheDepths,
  DepthsOfDemheTheHeightOfTheDepths (..),
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

newtype DepthsOfDemheTheHeightOfTheDepths = DepthsOfDemheTheHeightOfTheDepths LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData, HasAbilities)

instance HasModifiersFor DepthsOfDemheTheHeightOfTheDepths where
  getModifiersFor (InvestigatorTarget iid) (DepthsOfDemheTheHeightOfTheDepths a) = do
    here <- iid `isAt` a
    pure $ toModifiers a [CannotPlay FastCard | here]
  getModifiersFor _ _ = pure []

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
  runMessage msg (DepthsOfDemheTheHeightOfTheDepths attrs) = case msg of
    Flip iid _ target | isTarget attrs target -> do
      readStory iid (toId attrs) Story.theHeightOfTheDepths
      pure . DepthsOfDemheTheHeightOfTheDepths $ attrs & canBeFlippedL .~ False
    _ -> DepthsOfDemheTheHeightOfTheDepths <$> runMessage msg attrs
