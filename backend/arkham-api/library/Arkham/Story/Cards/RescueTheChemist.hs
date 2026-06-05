module Arkham.Story.Cards.RescueTheChemist (rescueTheChemist) where

import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype RescueTheChemist = RescueTheChemist StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

rescueTheChemist :: StoryCard RescueTheChemist
rescueTheChemist = story RescueTheChemist Cards.rescueTheChemist

instance RunMessage RescueTheChemist where
  runMessage msg (RescueTheChemist attrs) = RescueTheChemist <$> runMessage msg attrs
