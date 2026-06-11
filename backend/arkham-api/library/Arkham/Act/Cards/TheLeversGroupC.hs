module Arkham.Act.Cards.TheLeversGroupC (theLeversGroupC) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Card
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Scenario (scenarioField, scenarioFieldMap)
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Name (Labeled (..))
import Arkham.Scenario.Types (Field (..))
import Arkham.ScenarioLogKey
import Arkham.Scenarios.TheLabyrinthsOfLunacy.Helpers

newtype TheLeversGroupC = TheLeversGroupC ActAttrs
  deriving anyclass (IsAct, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- Timed - Do not advance this act until you are instructed. Before the
-- agenda advances, one investigator must activate the [action] ability on
-- the Chamber of Regret.
theLeversGroupC :: ActCard TheLeversGroupC
theLeversGroupC = act (1, A) TheLeversGroupC Cards.theLeversGroupC Nothing

-- Which lever matches the flavor text of the Chamber of Secrets placed
-- underneath the scenario reference card during setup.
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
  runMessage msg a@(TheLeversGroupC attrs) = runQueueT $ scenarioI18n $ scope "theLevers" $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      mPulled <- scenarioFieldMap ScenarioRemembered (find (isJust . leverPuller) . toList)
      case mPulled >>= \key -> fmap (\puller -> (key, puller)) (leverPuller key) of
        Just (key, puller) -> do
          hidden <- scenarioField ScenarioCardsUnderScenarioReference
          focusCards hidden (continue_ puller)
          let matched = any (matchingChamber key . toCardDef) hidden
          if matched
            then flavor $ h "title" >> p "match"
            else do
              flavor $ h "title" >> p "noMatch"
              push $ InvestigatorKilled (toSource attrs) puller
        Nothing -> do
          flavor $ h "title" >> p "noLever"
          nearest <- select $ NearestToLocation $ locationIs Locations.chamberOfRegret
          lead <- getLead
          chooseOrRunOneM lead do
            targets nearest \iid -> push $ InvestigatorKilled (toSource attrs) iid
      push $ ScenarioSpecific "act2Setup" Null
      advanceActDeck attrs
      pure a
    _ -> TheLeversGroupC <$> liftRunMessage msg attrs
