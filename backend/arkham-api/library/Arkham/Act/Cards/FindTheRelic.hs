module Arkham.Act.Cards.FindTheRelic (findTheRelic) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Asset.Cards qualified as Assets
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.ThreadsOfFate.Helpers

newtype FindTheRelic = FindTheRelic ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

findTheRelic :: ActCard FindTheRelic
findTheRelic = act (3, A) FindTheRelic Cards.findTheRelic Nothing

instance HasModifiersFor FindTheRelic where
  getModifiersFor (FindTheRelic a) =
    modifySelect a (LocationWithAsset $ assetIs Assets.relicOfAgesADeviceOfSomeSort) [ShroudModifier 2]

instance HasAbilities FindTheRelic where
  getAbilities = actAbilities1 \a ->
    restricted
      a
      1
      (exists $ locationWithAssetIs Assets.relicOfAgesADeviceOfSomeSort <> LocationWithoutClues)
      $ Objective
      $ forced AnyWindow

instance RunMessage FindTheRelic where
  runMessage msg a@(FindTheRelic attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      relicOfAges <- selectJust $ assetIs Assets.relicOfAgesADeviceOfSomeSort
      iids <- select $ NearestToLocation $ LocationWithAsset $ assetIs Assets.relicOfAgesADeviceOfSomeSort
      leadChooseOrRunOneM $ targets iids (`takeControlOfAsset` relicOfAges)

      deckCount <- getActDecksInPlayCount
      push
        $ if deckCount <= 1
          then R1
          else RemoveCompletedActFromGame (actDeckId attrs) (toId attrs)
      pure a
    _ -> FindTheRelic <$> liftRunMessage msg attrs
