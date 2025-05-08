module Arkham.Act.Cards.TheReallyBadOnesV1 (theReallyBadOnesV1) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Asset.Cards qualified as Assets
import Arkham.Card
import Arkham.Deck qualified as Deck
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Scenario
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenario.Types (Field (..))
import Arkham.Trait

newtype TheReallyBadOnesV1 = TheReallyBadOnesV1 ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

theReallyBadOnesV1 :: ActCard TheReallyBadOnesV1
theReallyBadOnesV1 = act (2, A) TheReallyBadOnesV1 Cards.theReallyBadOnesV1 Nothing

instance HasModifiersFor TheReallyBadOnesV1 where
  getModifiersFor (TheReallyBadOnesV1 attrs) = do
    modifySelect attrs UnrevealedLocation [TraitRestrictedModifier ArkhamAsylum Blank]

instance RunMessage TheReallyBadOnesV1 where
  runMessage msg a@(TheReallyBadOnesV1 attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      investigators <- select $ InvestigatorAt $ locationIs Locations.patientConfinementDanielsCell
      danielChesterfield <- fetchCard Assets.danielChesterfield
      leadChooseOneM do
        targets investigators (`takeControlOfSetAsideAsset` danielChesterfield)
      enemiesUnderAct <- filter ((== EnemyType) . toCardType) <$> scenarioField ScenarioCardsUnderActDeck
      shuffleCardsIntoDeck Deck.EncounterDeck enemiesUnderAct
      shuffleEncounterDiscardBackIn
      advanceActDeck attrs
      pure a
    _ -> TheReallyBadOnesV1 <$> liftRunMessage msg attrs
