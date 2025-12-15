module Arkham.Act.Cards.RecoverTheRelic (recoverTheRelic) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Asset.Cards qualified as Assets
import Arkham.Enemy.Types (Field (..))
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Placement
import Arkham.Projection
import Arkham.Scenarios.ThreadsOfFate.Helpers

newtype RecoverTheRelic = RecoverTheRelic ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

recoverTheRelic :: ActCard RecoverTheRelic
recoverTheRelic = act (3, A) RecoverTheRelic Cards.recoverTheRelic Nothing

instance HasModifiersFor RecoverTheRelic where
  getModifiersFor (RecoverTheRelic a) = do
    modifySelect a (EnemyWithAsset $ assetIs Assets.relicOfAgesADeviceOfSomeSort) [HealthModifier 2]

instance HasAbilities RecoverTheRelic where
  getAbilities = actAbilities1 \a ->
    mkAbility a 1
      $ Objective
      $ forced
      $ EnemyLeavesPlay #when
      $ EnemyWithModifier (ScenarioModifier "withRelicOfAges")

instance RunMessage RecoverTheRelic where
  runMessage msg a@(RecoverTheRelic attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      relicOfAges <-
        selectOne (assetIs Assets.relicOfAgesADeviceOfSomeSort)
          `orWhenNothingM` createAssetAt Assets.relicOfAgesADeviceOfSomeSort Unplaced
      findInvestigators <- runDefaultMaybeT Anyone do
        enemy <- MaybeT $ selectOne $ EnemyWithModifier (ScenarioModifier "withRelicOfAges")
        loc <- MaybeT (field EnemyLastKnownLocation enemy) <|> MaybeT (field EnemyLocation enemy)
        pure $ NearestToLocation $ LocationWithId loc

      iids <- select findInvestigators
      leadChooseOrRunOneM $ targets iids (`takeControlOfAsset` relicOfAges)
      deckCount <- getActDecksInPlayCount
      if deckCount <= 1
        then do
          selectEach (EnemyWithModifier (ScenarioModifier "withRelicOfAges")) addToVictoryIfNeeded
          push R1
        else push $ RemoveCompletedActFromGame (actDeckId attrs) (toId attrs)
      pure a
    _ -> RecoverTheRelic <$> liftRunMessage msg attrs
