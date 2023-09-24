module Arkham.Investigator.Cards.MandyThompson
  ( mandyThompson
  , MandyThompson(..)
  )
where

import Arkham.Prelude

import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Runner

newtype MandyThompson = MandyThompson InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mandyThompson :: InvestigatorCard MandyThompson
mandyThompson = investigator
  MandyThompson
  Cards.mandyThompson
  Stats
    { health = 6
    , sanity = 8
    , willpower = 3
    , intellect = 5
    , combat = 1
    , agility = 3
    }

instance HasAbilities MandyThompson where
  getAbilities (MandyThompson _) = []

instance HasChaosTokenValue MandyThompson where
  getChaosTokenValue iid ElderSign (MandyThompson attrs) | iid == toId attrs = do
    pure $ ChaosTokenValue ElderSign ZeroModifier
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage MandyThompson where
  runMessage msg (MandyThompson attrs) =
    MandyThompson <$> runMessage msg attrs
