module Arkham.Act.Cards.RecoverTheRelic (recoverTheRelic) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Asset.Cards qualified as Assets
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
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
  getAbilities (RecoverTheRelic a) =
    [ mkAbility a 1
        $ Objective
        $ forced
        $ EnemyLeavesPlay #when
        $ EnemyWithAsset (assetIs Assets.relicOfAgesADeviceOfSomeSort)
    ]

instance RunMessage RecoverTheRelic where
  runMessage msg a@(RecoverTheRelic attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      relicOfAges <- selectJust $ assetIs Assets.relicOfAgesADeviceOfSomeSort
      iids <- select $ NearestToEnemy $ EnemyWithAsset $ assetIs Assets.relicOfAgesADeviceOfSomeSort
      leadChooseOrRunOneM $ targets iids (`takeControlOfAsset` relicOfAges)
      deckCount <- getActDecksInPlayCount
      push
        $ if deckCount <= 1
          then R1
          else RemoveCompletedActFromGame (actDeckId attrs) (toId attrs)
      pure a
    _ -> RecoverTheRelic <$> liftRunMessage msg attrs
