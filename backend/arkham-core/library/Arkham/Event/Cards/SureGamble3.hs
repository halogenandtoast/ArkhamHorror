module Arkham.Event.Cards.SureGamble3 where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Effect.Runner
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Modifiers
import Arkham.Message hiding (RevealChaosToken)
import Arkham.Window

newtype SureGamble3 = SureGamble3 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sureGamble3 :: EventCard SureGamble3
sureGamble3 = event SureGamble3 Cards.sureGamble3

instance RunMessage SureGamble3 where
  runMessage msg e@(SureGamble3 attrs) = case msg of
    InvestigatorPlayEvent _ eid _ (revealedChaosTokens -> [token]) _ | attrs `is` eid -> do
      unshiftEffect attrs (ChaosTokenTarget token)
      pure e
    _ -> SureGamble3 <$> runMessage msg attrs

newtype SureGamble3Effect = SureGamble3Effect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sureGamble3Effect :: EffectArgs -> SureGamble3Effect
sureGamble3Effect = cardEffect SureGamble3Effect Cards.sureGamble3

instance HasModifiersFor SureGamble3Effect where
  getModifiersFor target (SureGamble3Effect a) | a.target `is` target = do
    pure $ toModifiers a [NegativeToPositive]
  getModifiersFor _ _ = pure []

instance RunMessage SureGamble3Effect where
  runMessage msg e@(SureGamble3Effect attrs) = case msg of
    SkillTestEnds _ _ -> e <$ push (disable attrs)
    _ -> SureGamble3Effect <$> runMessage msg attrs
