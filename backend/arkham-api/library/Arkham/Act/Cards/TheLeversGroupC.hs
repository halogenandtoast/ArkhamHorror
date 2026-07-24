module Arkham.Act.Cards.TheLeversGroupC (theLeversGroupC) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Card
import Arkham.Helpers.Scenario (scenarioField, scenarioFieldMap)
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Name (Labeled (..))
import Arkham.Scenario.Types (Field (..))
import Arkham.ScenarioLogKey

newtype TheLeversGroupC = TheLeversGroupC ActAttrs
  deriving anyclass (IsAct, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theLeversGroupC :: ActCard TheLeversGroupC
theLeversGroupC = act (1, A) TheLeversGroupC Cards.theLeversGroupC Nothing

matchingChamber :: ScenarioLogKey -> CardDef -> Bool
matchingChamber key def = case key of
  PulledTheRightLever _ -> def == Locations.chamberOfSecretsBloodyPrison
  PulledTheMiddleLever _ -> def == Locations.chamberOfSecretsMysteriousPrison
  PulledTheLeftLever _ -> def == Locations.chamberOfSecretsEnshroudedPrison
  _ -> False

leverPuller :: ScenarioLogKey -> Maybe InvestigatorId
leverPuller = \case
  PulledTheLeftLever (Labeled _ iid) -> Just iid
  PulledTheMiddleLever (Labeled _ iid) -> Just iid
  PulledTheRightLever (Labeled _ iid) -> Just iid
  _ -> Nothing

instance RunMessage TheLeversGroupC where
  runMessage msg a@(TheLeversGroupC attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      mPulled <- scenarioFieldMap ScenarioRemembered (find (isJust . leverPuller) . toList)
      case mPulled >>= \key -> fmap (key,) (leverPuller key) of
        Just (key, puller) -> do
          hidden <- traverse flippedOverCapture =<< scenarioField ScenarioCardsUnderScenarioReference
          focusCards hidden (continue_ puller)
          let matched = any (matchingChamber key . toCardDef) hidden
          unless matched do
            push $ InvestigatorKilled (toSource attrs) puller
        Nothing -> do
          nearest <- select $ NearestToLocation $ locationIs Locations.chamberOfRegret
          lead <- getLead
          chooseOrRunOneM lead $ targets nearest $ push . InvestigatorKilled (toSource attrs)
      scenarioSpecific_ "act2Setup"
      advanceActDeck attrs
      pure a
    _ -> TheLeversGroupC <$> liftRunMessage msg attrs
