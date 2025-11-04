module Arkham.Act.Cards.ObtainingTheDevice (obtainingTheDevice) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Asset.Cards qualified as Assets
import Arkham.Helpers.Query
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Placement

newtype ObtainingTheDevice = ObtainingTheDevice ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

obtainingTheDevice :: ActCard ObtainingTheDevice
obtainingTheDevice = act (2, A) ObtainingTheDevice Cards.obtainingTheDevice Nothing

instance HasAbilities ObtainingTheDevice where
  getAbilities (ObtainingTheDevice attrs) =
    [mkAbility attrs 1 $ Objective $ forced $ AddedToVictory #at $ CardWithTitle "Nathan Wick"]

instance RunMessage ObtainingTheDevice where
  runMessage msg a@(ObtainingTheDevice attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      investigators <- getInvestigators
      puzzleBox <-
        selectOne (assetIs Assets.puzzleBox) `orWhenNothingM` createAssetAt Assets.puzzleBox Unplaced
      leadChooseOrRunOneM $ targets investigators (`takeControlOfAsset` puzzleBox)
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> ObtainingTheDevice <$> liftRunMessage msg attrs
