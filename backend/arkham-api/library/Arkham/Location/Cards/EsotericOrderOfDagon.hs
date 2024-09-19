module Arkham.Location.Cards.EsotericOrderOfDagon (esotericOrderOfDagon, EsotericOrderOfDagon (..)) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenario.Deck
import Arkham.Scenarios.TheVanishingOfElinaHarper.Helpers

newtype EsotericOrderOfDagon = EsotericOrderOfDagon LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

esotericOrderOfDagon :: LocationCard EsotericOrderOfDagon
esotericOrderOfDagon = location EsotericOrderOfDagon Cards.esotericOrderOfDagon 3 (PerPlayer 1)

instance HasAbilities EsotericOrderOfDagon where
  getAbilities (EsotericOrderOfDagon a) =
    extendRevealed1 a
      $ restrictedAbility a 1 (ScenarioDeckWithCard LeadsDeck)
      $ forced
      $ DiscoveringLastClue #after Anyone (be a)

instance RunMessage EsotericOrderOfDagon where
  runMessage msg l@(EsotericOrderOfDagon attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      cards <- take 1 <$> getLeadsDeck
      for_ cards obtainCard
      pure l
    _ -> EsotericOrderOfDagon <$> liftRunMessage msg attrs
