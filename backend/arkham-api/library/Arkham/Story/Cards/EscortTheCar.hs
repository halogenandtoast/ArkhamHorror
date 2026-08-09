module Arkham.Story.Cards.EscortTheCar (escortTheCar) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Query
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Placement
import Arkham.ScenarioLogKey
import Arkham.Scenarios.TheBlobThatAteEverything.Helpers
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype EscortTheCar = EscortTheCar StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

escortTheCar :: StoryCard EscortTheCar
escortTheCar = story EscortTheCar Cards.escortTheCar & persistStory

instance HasAbilities EscortTheCar where
  getAbilities (EscortTheCar a) =
    [ restricted a 2 (not_ $ Remembered TheCarReachedItsTarget)
        $ forced
        $ AssetLeavesPlay #when (assetIs Assets.armoredCar)
    ]

instance RunMessage EscortTheCar where
  runMessage msg s@(EscortTheCar attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      fungus <- getJustLocationByName "Fungus Mound"
      createEnemyAt_ Enemies.miGoDestroyer fungus
      car <- getSetAsideCard Assets.armoredCar
      locations <- select $ FarthestLocationFromAll LocationCanHaveAttachments
      leadChooseOrRunOneM $ targets locations $ createAssetAt_ car . AtLocation
      pure $ EscortTheCar $ attrs & placementL .~ Global
    UseThisAbility iid (isSource attrs -> True) 2 -> remember TheEscortFailed >> flipOver iid attrs >> pure s
    Flip iid _ (isTarget attrs -> True) -> do
      chooseOneM iid $ targeting attrs nothing
      selectEach (assetIs Assets.armoredCar) removeFromGame
      succeeded <- remembered TheCarReachedItsTarget
      if succeeded
        then do
          reward <- getSetAsideCard Assets.gMen
          investigators <- allInvestigators
          leadChooseOrRunOneM $ portraits investigators (`takeControlOfSetAsideAsset` reward)
          selectEach (enemyIs Enemies.miGoDestroyer) (addToVictory iid)
          addToVictory iid attrs
          getSubject8L08 >>= traverse_ (nonAttackEnemyDamage (Just iid) (attrs.ability 1) 5)
        else do
          selectEach (enemyIs Enemies.miGoDestroyer) removeFromGame
          removeFromGame attrs
          eachInvestigator \iid' -> assignDamage iid' (attrs.ability 2) 1
          removeTokens (attrs.ability 2) ScenarioTarget #resource 1
      pure $ EscortTheCar $ attrs & flippedL .~ True
    _ -> EscortTheCar <$> liftRunMessage msg attrs
