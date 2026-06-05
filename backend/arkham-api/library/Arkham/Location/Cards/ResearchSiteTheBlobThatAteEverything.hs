module Arkham.Location.Cards.ResearchSiteTheBlobThatAteEverything (researchSiteTheBlobThatAteEverything) where

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype ResearchSiteTheBlobThatAteEverything = ResearchSiteTheBlobThatAteEverything LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

researchSiteTheBlobThatAteEverything :: LocationCard ResearchSiteTheBlobThatAteEverything
researchSiteTheBlobThatAteEverything = locationWith ResearchSiteTheBlobThatAteEverything Cards.researchSiteTheBlobThatAteEverything 2 (Static 0) connectsToAdjacent

instance RunMessage ResearchSiteTheBlobThatAteEverything where
  runMessage msg (ResearchSiteTheBlobThatAteEverything attrs) = ResearchSiteTheBlobThatAteEverything <$> runMessage msg attrs
