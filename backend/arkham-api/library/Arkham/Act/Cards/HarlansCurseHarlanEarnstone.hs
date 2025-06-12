module Arkham.Act.Cards.HarlansCurseHarlanEarnstone (harlansCurseHarlanEarnstone) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Acts
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Asset.Cards qualified as Assets
import Arkham.Asset.Types (Field (..))
import Arkham.Card
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Matcher hiding (AssetCard)
import Arkham.Placement
import Arkham.Projection

newtype HarlansCurseHarlanEarnstone = HarlansCurseHarlanEarnstone ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

harlansCurseHarlanEarnstone :: ActCard HarlansCurseHarlanEarnstone
harlansCurseHarlanEarnstone = act (2, A) HarlansCurseHarlanEarnstone Cards.harlansCurseHarlanEarnstone Nothing

instance HasAbilities HarlansCurseHarlanEarnstone where
  getAbilities = actAbilities1 \a ->
    restricted a 1 (exists $ assetIs Assets.harlanEarnstone <> AssetWithClues (AtLeast $ PerPlayer 1))
      $ Objective
      $ forced AnyWindow

instance RunMessage HarlansCurseHarlanEarnstone where
  runMessage msg a@(HarlansCurseHarlanEarnstone attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      harlan <- selectJust $ assetIs Assets.harlanEarnstone
      harlansLocation <- selectJust $ locationWithAsset harlan
      cardId <- field AssetCardId harlan
      let harlanEarnstoneCrazedByTheCurse = lookupCard Enemies.harlanEarnstoneCrazedByTheCurse cardId
      createEnemyAt_ harlanEarnstoneCrazedByTheCurse harlansLocation
      push $ Flipped (toSource harlan) harlanEarnstoneCrazedByTheCurse
      doStep 1 msg
      advanceToAct attrs Acts.recoverTheRelic A
      pure a
    DoStep 1 (AdvanceAct (isSide B attrs -> True) _ _) -> do
      harlan <- selectJust $ enemyIs Enemies.harlanEarnstoneCrazedByTheCurse
      createAssetAt_ Assets.relicOfAgesADeviceOfSomeSort (AttachedToEnemy harlan)
      pure a
    _ -> HarlansCurseHarlanEarnstone <$> liftRunMessage msg attrs
