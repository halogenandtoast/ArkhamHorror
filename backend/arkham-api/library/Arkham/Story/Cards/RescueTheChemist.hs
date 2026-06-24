module Arkham.Story.Cards.RescueTheChemist (rescueTheChemist) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Query
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Placement
import Arkham.ScenarioLogKey
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted
import Arkham.Trait (Trait (Ooze))

newtype RescueTheChemist = RescueTheChemist StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

rescueTheChemist :: StoryCard RescueTheChemist
rescueTheChemist = story RescueTheChemist Cards.rescueTheChemist & persistStory

instance HasAbilities RescueTheChemist where
  getAbilities (RescueTheChemist a) =
    [ mkAbility a 1
        $ forced
        $ Moves
          #after
          (ControlsAsset $ assetIs Assets.universityChemist)
          AnySource
          Anywhere
          (LocationWithTitle "Temporary HQ")
    , -- "defeated or devoured" only matters while the chemist has not been
      -- saved; without this guard the saved branch's removeFromGame cleanup
      -- would re-trigger this and wrongly remember "the formula was not completed".
      restricted a 2 (not_ $ Remembered TheChemistWasSaved)
        $ forced
        $ AssetLeavesPlay #when (assetIs Assets.universityChemist)
    ]

instance RunMessage RescueTheChemist where
  runMessage msg s@(RescueTheChemist attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      fungusMound <- getJustLocationByName "Fungus Mound"
      createEnemyAt_ Enemies.miGoAbductor fungusMound
      chemist <- getSetAsideCard Assets.universityChemist
      locations <- select $ FarthestLocationFromAll LocationCanHaveAttachments
      leadChooseOrRunOneM $ targets locations $ createAssetAt_ chemist . AtLocation
      pure $ RescueTheChemist $ attrs & placementL .~ Global
    ScenarioSpecific "devour" (maybeResult -> Just (AssetTarget aid)) -> do
      whenM (not <$> remembered TheChemistWasSaved) do
        whenM (aid <=~> assetIs Assets.universityChemist) do
          lead <- getLead
          push $ UseCardAbility lead (toSource attrs) 2 [] NoPayment
      pure s
    ScenarioSpecific "devour" (maybeResult -> Just (CardIdTarget cid)) -> do
      whenM (not <$> remembered TheChemistWasSaved) do
        whenM (selectAny $ AssetWithCardId cid <> assetIs Assets.universityChemist) do
          lead <- getLead
          push $ UseCardAbility lead (toSource attrs) 2 [] NoPayment
      pure s
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      remember TheChemistWasSaved
      flipOver iid attrs
      pure s
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      remember TheFormulaWasNotCompleted
      flipOver iid attrs
      pure s
    Flip iid _ (isTarget attrs -> True) -> do
      chooseOneM iid $ targeting attrs nothing

      selectEach (assetIs Assets.universityChemist) removeFromGame

      whenM (remembered TheChemistWasSaved) do
        solvent <- getSetAsideCard Assets.universalSolvent
        investigators <- allInvestigators
        leadChooseOrRunOneM $ withI18n do
          nameVar Assets.universalSolvent $ questionLabeled' "takeControlOf"
          questionLabeledCard Assets.universalSolvent
          portraits investigators (`takeControlOfSetAsideAsset` solvent)
        selectEach (enemyIs Enemies.miGoAbductor) (addToVictory iid)
        addToVictory iid attrs
        enemies <- select $ EnemyWithTrait Ooze
        chooseTargetM iid enemies $ nonAttackEnemyDamage (Just iid) (attrs.ability 1) 3

      whenM (remembered TheFormulaWasNotCompleted) do
        selectEach (enemyIs Enemies.miGoAbductor) removeFromGame
        removeFromGame attrs
        eachInvestigator \iid' -> assignHorror iid' (attrs.ability 2) 1
        removeTokens (attrs.ability 2) ScenarioTarget #resource 1

      pure $ RescueTheChemist $ attrs & flippedL .~ True
    _ -> RescueTheChemist <$> liftRunMessage msg attrs
