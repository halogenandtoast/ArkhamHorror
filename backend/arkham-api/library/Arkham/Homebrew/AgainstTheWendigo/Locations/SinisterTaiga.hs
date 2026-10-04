module Arkham.Homebrew.AgainstTheWendigo.Locations.SinisterTaiga (sinisterTaiga) where

import Arkham.Helpers.Modifiers
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype SinisterTaiga = SinisterTaiga LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

sinisterTaiga :: LocationCard SinisterTaiga
sinisterTaiga = location SinisterTaiga Cards.sinisterTaiga 2 (PerPlayer 1)

instance HasModifiersFor SinisterTaiga where
  -- "While you are in this location, you lose -1 [willpower]."
  getModifiersFor (SinisterTaiga a) =
    modifySelect a (InvestigatorAt $ be a) [SkillModifier #willpower (-1)]

instance RunMessage SinisterTaiga where
  runMessage msg (SinisterTaiga attrs) = runQueueT $ SinisterTaiga <$> liftRunMessage msg attrs
