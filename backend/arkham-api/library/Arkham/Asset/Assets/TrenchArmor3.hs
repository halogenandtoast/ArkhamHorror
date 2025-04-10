module Arkham.Asset.Assets.TrenchArmor3 (trenchArmor3) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Modifiers (ModifierType (..), controllerGetsMaybe)
import Arkham.History

newtype TrenchArmor3 = TrenchArmor3 AssetAttrs
  deriving anyclass (IsAsset, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

trenchArmor3 :: AssetCard TrenchArmor3
trenchArmor3 = assetWith TrenchArmor3 Cards.trenchArmor3 (healthL ?~ 3)

instance HasModifiersFor TrenchArmor3 where
  getModifiersFor (TrenchArmor3 a) = controllerGetsMaybe a \iid -> do
    n <- lift $ getHistoryField RoundHistory iid HistoryAttacksOfOpportunity
    guard $ n == 0 && getAssetMetaDefault True a
    pure [IgnoreAttacksOfOpportunity]

instance RunMessage TrenchArmor3 where
  runMessage msg a@(TrenchArmor3 attrs) = runQueueT $ case msg of
    BeginRound -> pure $ TrenchArmor3 $ attrs & setMeta True
    EnemyAttack details -> do
      case details.investigator of
        Just iid | Just iid == attrs.controller -> pure $ TrenchArmor3 $ attrs & setMeta False
        _ -> pure a
    _ -> TrenchArmor3 <$> liftRunMessage msg attrs
