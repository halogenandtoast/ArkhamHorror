module Arkham.Asset.Cards.TrueMagickReworkingReality5 (
  trueMagickReworkingReality5,
  TrueMagickReworkingReality5 (..),
)
where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Helpers.Modifiers

newtype TrueMagickReworkingReality5 = TrueMagickReworkingReality5 AssetAttrs
  deriving anyclass (IsAsset, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

trueMagickReworkingReality5 :: AssetCard TrueMagickReworkingReality5
trueMagickReworkingReality5 = asset TrueMagickReworkingReality5 Cards.trueMagickReworkingReality5

instance HasModifiersFor TrueMagickReworkingReality5 where
  getModifiersFor (InvestigatorTarget iid) (TrueMagickReworkingReality5 attrs)
    | attrs `controlledBy` iid = pure $ toModifiers attrs [TrueMagick]
  getModifiersFor _ _ = pure []

instance RunMessage TrueMagickReworkingReality5 where
  runMessage msg (TrueMagickReworkingReality5 attrs) = runQueueT $ case msg of
    Do BeginRound -> pure . TrueMagickReworkingReality5 $ attrs & usesL . ix Charge %~ max 1
    _ -> TrueMagickReworkingReality5 <$> lift (runMessage msg attrs)
