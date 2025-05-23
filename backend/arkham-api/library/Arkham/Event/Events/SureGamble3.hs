module Arkham.Event.Events.SureGamble3 (sureGamble3) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Modifier
import Arkham.Window

newtype SureGamble3 = SureGamble3 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sureGamble3 :: EventCard SureGamble3
sureGamble3 = event SureGamble3 Cards.sureGamble3

instance RunMessage SureGamble3 where
  runMessage msg e@(SureGamble3 attrs) = runQueueT $ case msg of
    PlayThisEvent _ (is attrs -> True) -> do
      withSkillTest \sid ->
        for_ (revealedChaosTokens attrs.windows) \token ->
          skillTestModifier sid attrs (ChaosTokenTarget token) NegativeToPositive
      pure e
    _ -> SureGamble3 <$> liftRunMessage msg attrs
