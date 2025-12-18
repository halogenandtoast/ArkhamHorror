module Arkham.Act.Cards.SecretsAndLiesV2 (secretsAndLiesV2) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Agenda.Types (Field (..))
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Modifiers (ModifierType (..), modified_)
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Grid
import Arkham.Matcher
import Arkham.Message.Lifted.Move
import Arkham.Message.Lifted.Placement
import Arkham.Phase
import Arkham.Projection
import Arkham.Scenarios.CongressOfTheKeys.Helpers

newtype SecretsAndLiesV2 = SecretsAndLiesV2 ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

secretsAndLiesV2 :: ActCard SecretsAndLiesV2
secretsAndLiesV2 = act (1, A) SecretsAndLiesV2 Cards.secretsAndLiesV2 Nothing

instance HasModifiersFor SecretsAndLiesV2 where
  getModifiersFor (SecretsAndLiesV2 a) = do
    modified_ a (PhaseTarget #mythos) [SkipMythosPhaseStep EachInvestigatorDrawsEncounterCardStep]

instance HasAbilities SecretsAndLiesV2 where
  getAbilities = actAbilities1 \a ->
    restricted a 1 (EachUndefeatedInvestigator $ at_ $ LocationWithInvestigator LeadInvestigator)
      $ Objective
      $ freeReaction (RoundEnds #when)

instance RunMessage SecretsAndLiesV2 where
  runMessage msg a@(SecretsAndLiesV2 attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      agenda <- selectJust AnyAgenda
      doom <- field AgendaDoom agenda
      advanceToAgendaA attrs Agendas.theWorldUnbidden
      placeDoomOnAgenda doom
      eachInvestigator (`place` Unplaced)
      theRedGlovedMan <- selectJust (enemyIs Enemies.theRedGlovedManPurposeUnknown)
      place theRedGlovedMan Unplaced
      selectEach ConcealedCardAny removeFromGame
      selectEach Anywhere removeLocation
      theKnottedTower <- placeLocationCardInGrid (Pos 0 0) Locations.theKnottedTower
      placeLocationCardInGrid_ (Pos 0 1) Locations.gravityDefyingClimb
      theToweringVertex <- placeLocationCardInGrid (Pos 0 2) Locations.theToweringVertexRuinousConflux
      moveAllTo attrs theKnottedTower
      place theRedGlovedMan theToweringVertex
      scenarioSpecific "setupOtherworld" Version2
      advanceActDeck attrs
      pure a
    _ -> SecretsAndLiesV2 <$> liftRunMessage msg attrs
