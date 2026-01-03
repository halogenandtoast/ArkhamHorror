module Arkham.Asset.Assets.BertieMusgraveATrueAesthete (bertieMusgraveATrueAesthete) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype BertieMusgraveATrueAesthete = BertieMusgraveATrueAesthete AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bertieMusgraveATrueAesthete :: AssetCard BertieMusgraveATrueAesthete
bertieMusgraveATrueAesthete = asset BertieMusgraveATrueAesthete Cards.bertieMusgraveATrueAesthete

instance RunMessage BertieMusgraveATrueAesthete where
  runMessage msg (BertieMusgraveATrueAesthete attrs) = runQueueT $ case msg of
    _ -> BertieMusgraveATrueAesthete <$> liftRunMessage msg attrs
