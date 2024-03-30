module Arkham.Investigator.Cards.SisterMary (
  sisterMary,
  SisterMary (..),
)
where

import Arkham.Helpers.ChaosBag
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Runner
import Arkham.Matcher
import Arkham.Prelude

newtype SisterMary = SisterMary InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sisterMary :: InvestigatorCard SisterMary
sisterMary =
  investigator
    SisterMary
    Cards.sisterMary
    Stats
      { health = 5
      , sanity = 9
      , willpower = 4
      , intellect = 2
      , combat = 3
      , agility = 3
      }

instance HasAbilities SisterMary where
  getAbilities (SisterMary attrs) = [restrictedAbility attrs 1 (Self <> HasRemainingBlessTokens) $ freeReaction $ RoundEnds #when]

instance HasChaosTokenValue SisterMary where
  getChaosTokenValue iid ElderSign (SisterMary attrs) | iid == toId attrs = do
    pure $ ChaosTokenValue ElderSign (PositiveModifier 1)
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage SisterMary where
  runMessage msg i@(SisterMary attrs) = case msg of
    Setup -> do
      push $ AddChaosToken BlessToken
      push $ AddChaosToken BlessToken
      SisterMary <$> runMessage msg attrs
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      push $ AddChaosToken BlessToken
      pure i
    PassedSkillTestWithToken iid ElderSign | attrs `is` iid -> do
      n <- getRemainingBlessTokens
      pushWhen (n > 0) $ AddChaosToken BlessToken
      pure i
    _ -> SisterMary <$> runMessage msg attrs
