module Arkham.Act.Cards.SecretsAndLiesV1 (secretsAndLiesV1) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Campaigns.TheScarletKeys.Concealed.Kind
import Arkham.Scenarios.CongressOfTheKeys.Helpers
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelectWith)
import Arkham.Keyword qualified as Keyword
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Grid
import Arkham.Matcher
import Arkham.Message.Lifted.Move
import Arkham.Message.Lifted.Placement
import Arkham.Modifier (setActiveDuringSetup)

newtype SecretsAndLiesV1 = SecretsAndLiesV1 ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

secretsAndLiesV1 :: ActCard SecretsAndLiesV1
secretsAndLiesV1 = act (1, A) SecretsAndLiesV1 Cards.secretsAndLiesV1 Nothing

instance HasModifiersFor SecretsAndLiesV1 where
  getModifiersFor (SecretsAndLiesV1 a) = do
    modifySelectWith
      a
      (enemyIs Enemies.theRedGlovedManPurposeUnknown)
      setActiveDuringSetup
      [AddKeyword $ Keyword.Concealed TheRedGlovedMan (PerPlayer 1), CannotBeDamaged]

instance HasAbilities SecretsAndLiesV1 where
  getAbilities = actAbilities1 \a ->
    restricted a 1 (EachUndefeatedInvestigator $ at_ locationWithTheRedGlovedMan)
      $ Objective
      $ triggered (RoundEnds #when) (GroupClueCost (PerPlayer 2) locationWithTheRedGlovedMan)
   where
    locationWithTheRedGlovedMan = LocationWithEnemy $ enemyIs Enemies.theRedGlovedManPurposeUnknown

instance RunMessage SecretsAndLiesV1 where
  runMessage msg a@(SecretsAndLiesV1 attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advancedWithClues attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
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
      scenarioSpecific "setupOtherworld" Version1
      advanceActDeck attrs
      pure a
    _ -> SecretsAndLiesV1 <$> liftRunMessage msg attrs
