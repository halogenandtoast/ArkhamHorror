module Arkham.Investigator.Cards.JennyBarnes where

import Arkham.Helpers.Modifiers
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Runner
import Arkham.Prelude

newtype JennyBarnes = JennyBarnes InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving stock (Data)

jennyBarnes :: InvestigatorCard JennyBarnes
jennyBarnes =
  investigator JennyBarnes Cards.jennyBarnes
    $ Stats {health = 8, sanity = 7, willpower = 3, intellect = 3, combat = 3, agility = 3}

instance HasModifiersFor JennyBarnes where
  getModifiersFor target (JennyBarnes attrs) | attrs `is` target = do
    pure $ toModifiers attrs [UpkeepResources 1]
  getModifiersFor _ _ = pure []

instance HasChaosTokenValue JennyBarnes where
  getChaosTokenValue iid ElderSign (JennyBarnes attrs) | attrs `is` iid = do
    pure $ ChaosTokenValue ElderSign (PositiveModifier attrs.resources)
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage JennyBarnes where
  runMessage msg (JennyBarnes attrs) = JennyBarnes <$> runMessage msg attrs
