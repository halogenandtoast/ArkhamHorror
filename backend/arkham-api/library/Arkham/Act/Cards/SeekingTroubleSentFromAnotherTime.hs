module Arkham.Act.Cards.SeekingTroubleSentFromAnotherTime (seekingTroubleSentFromAnotherTime) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaigns.TheForgottenAge.Key
import Arkham.Card
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Creation
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Log
import Arkham.Matcher

newtype SeekingTroubleSentFromAnotherTime = SeekingTroubleSentFromAnotherTime ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

seekingTroubleSentFromAnotherTime :: ActCard SeekingTroubleSentFromAnotherTime
seekingTroubleSentFromAnotherTime = act (2, G) SeekingTroubleSentFromAnotherTime Cards.seekingTroubleSentFromAnotherTime Nothing

instance HasAbilities SeekingTroubleSentFromAnotherTime where
  getAbilities = actAbilities1' G \a ->
    restricted
      a
      1
      (exists $ assetIs Assets.merleGarvinUnhelpfulGuide <> AssetWithClues (AtLeast $ PerPlayer 1))
      $ Objective
      $ forced AnyWindow

instance RunMessage SeekingTroubleSentFromAnotherTime where
  runMessage msg a@(SeekingTroubleSentFromAnotherTime attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      n <- perPlayer 1
      selectEach (assetIs Assets.merleGarvinUnhelpfulGuide) (removeCluesFrom (attrs.ability 1) n)
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide H attrs -> True) _ _ -> do
      harbinger <- genCard Enemies.harbingerOfValusiaTheSleeperReturns
      merlesLocation <- selectJust $ LocationWithAsset $ assetIs Assets.merleGarvinUnhelpfulGuide
      createEnemyWithM_ harbinger merlesLocation \create -> do
        yigsFury <- getRecordCount YigsFury
        let handleCreate = if yigsFury <= 5 then createExhausted else id
        setAfter (handleCreate create) do
          startingDamage <- getRecordCount TheHarbingerIsStillAlive
          when (startingDamage > 0) $ placeTokens attrs create.enemy #damage startingDamage
      advanceToAct attrs Cards.impossiblePursuit G
      pure a
    _ -> SeekingTroubleSentFromAnotherTime <$> liftRunMessage msg attrs
