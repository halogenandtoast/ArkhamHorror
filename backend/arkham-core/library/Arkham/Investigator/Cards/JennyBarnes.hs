module Arkham.Investigator.Cards.JennyBarnes where

import Arkham.Prelude

import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Runner
import Arkham.Message
import Arkham.Token

newtype JennyBarnes = JennyBarnes InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

jennyBarnes :: InvestigatorCard JennyBarnes
jennyBarnes =
  investigator
    JennyBarnes
    Cards.jennyBarnes
    Stats
      { health = 8
      , sanity = 7
      , willpower = 3
      , intellect = 3
      , combat = 3
      , agility = 3
      }

instance HasChaosTokenValue JennyBarnes where
  getChaosTokenValue iid ElderSign (JennyBarnes attrs) | iid == investigatorId attrs = do
    pure $ ChaosTokenValue ElderSign (PositiveModifier $ investigatorResources attrs)
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage JennyBarnes where
  runMessage msg (JennyBarnes attrs) = case msg of
    -- TODO: Move this to a modifier
    AllDrawCardAndResource
      | not (attrs ^. defeatedL || attrs ^. resignedL) ->
          JennyBarnes <$> runMessage msg (attrs & tokensL %~ incrementTokens Resource)
    _ -> JennyBarnes <$> runMessage msg attrs
