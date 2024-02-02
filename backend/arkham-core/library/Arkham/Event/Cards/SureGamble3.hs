module Arkham.Event.Cards.SureGamble3 where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Modifiers
import Arkham.Window

newtype SureGamble3 = SureGamble3 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

sureGamble3 :: EventCard SureGamble3
sureGamble3 = event SureGamble3 Cards.sureGamble3

instance RunMessage SureGamble3 where
  runMessage msg e@(SureGamble3 attrs) = case msg of
    InvestigatorPlayEvent _ eid _ (revealedChaosTokens -> [token]) _ | attrs `is` eid -> do
      push $ skillTestModifier attrs (ChaosTokenTarget token) NegativeToPositive
      pure e
    _ -> SureGamble3 <$> runMessage msg attrs
