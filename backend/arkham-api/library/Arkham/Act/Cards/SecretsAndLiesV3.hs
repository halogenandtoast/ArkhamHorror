module Arkham.Act.Cards.SecretsAndLiesV3 (secretsAndLiesV3) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaigns.TheScarletKeys.Concealed.Kind
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelectWith)
import Arkham.Keyword qualified as Keyword
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Grid
import Arkham.Matcher
import Arkham.Message.Lifted.Move
import Arkham.Message.Lifted.Placement
import Arkham.Modifier (setActiveDuringSetup)

newtype SecretsAndLiesV3 = SecretsAndLiesV3 ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

secretsAndLiesV3 :: ActCard SecretsAndLiesV3
secretsAndLiesV3 = act (1, A) SecretsAndLiesV3 Cards.secretsAndLiesV3 Nothing

instance HasAbilities SecretsAndLiesV3 where
  getAbilities = actAbilities1 \a ->
    restricted a 1 (EachUndefeatedInvestigator $ at_ locationWithTheRedGlovedMan)
      $ Objective
      $ triggered (RoundEnds #when) (GroupClueCost (PerPlayer 2) locationWithTheRedGlovedMan)
   where
    locationWithTheRedGlovedMan =
      oneOf
        [ LocationWithEnemy
            $ enemyIs Enemies.theRedGlovedManPurposeUnknown
        , LocationWithAsset $ assetIs Assets.theRedGlovedManHeWasAlwaysThere
        ]

instance HasModifiersFor SecretsAndLiesV3 where
  getModifiersFor (SecretsAndLiesV3 a) = do
    modifySelectWith
      a
      (enemyIs Enemies.theRedGlovedManPurposeUnknown)
      setActiveDuringSetup
      [AddKeyword $ Keyword.Concealed TheRedGlovedMan (PerPlayer 1), CannotBeDamaged]

instance RunMessage SecretsAndLiesV3 where
  runMessage msg a@(SecretsAndLiesV3 attrs) = runQueueT $ case msg of
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
      scenarioSpecific_ "setupOtherworld"
      advanceActDeck attrs
      pure a
    _ -> SecretsAndLiesV3 <$> liftRunMessage msg attrs
