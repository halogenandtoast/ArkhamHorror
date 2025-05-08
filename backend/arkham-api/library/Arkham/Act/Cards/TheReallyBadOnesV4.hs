module Arkham.Act.Cards.TheReallyBadOnesV4 (theReallyBadOnesV4) where

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
import Arkham.Placement
import Arkham.Scenario.Types (Field (..))
import Arkham.Trait
import Arkham.Treachery.Cards qualified as Treacheries

newtype TheReallyBadOnesV4 = TheReallyBadOnesV4 ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

theReallyBadOnesV4 :: ActCard TheReallyBadOnesV4
theReallyBadOnesV4 = act (2, A) TheReallyBadOnesV4 Cards.theReallyBadOnesV4 Nothing

instance HasModifiersFor TheReallyBadOnesV4 where
  getModifiersFor (TheReallyBadOnesV4 attrs) = do
    modifySelect attrs UnrevealedLocation [TraitRestrictedModifier ArkhamAsylum Blank]

instance RunMessage TheReallyBadOnesV4 where
  runMessage msg a@(TheReallyBadOnesV4 attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      investigators <- select $ InvestigatorAt $ locationIs Locations.patientConfinementDanielsCell
      danielChesterfield <- fetchCard Assets.danielChesterfield
      leadChooseOneM do
        targets investigators (`takeControlOfSetAsideAsset` danielChesterfield)
      doStep 1 msg
      enemiesUnderAct <- filter ((== EnemyType) . toCardType) <$> scenarioField ScenarioCardsUnderActDeck
      shuffleCardsIntoDeck Deck.EncounterDeck enemiesUnderAct
      shuffleEncounterDiscardBackIn
      advanceActDeck attrs
      pure a
    DoStep 1 (AdvanceAct (isSide B attrs -> True) _ _) -> do
      selectOne (assetIs Assets.danielChesterfield) >>= traverse_ \aid -> do
        createTreacheryAt_ Treacheries.radicalTreatment (AttachedToAsset aid Nothing)
      pure a
    _ -> TheReallyBadOnesV4 <$> liftRunMessage msg attrs
