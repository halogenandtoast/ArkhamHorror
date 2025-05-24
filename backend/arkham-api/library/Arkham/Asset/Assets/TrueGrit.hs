module Arkham.Asset.Assets.TrueGrit (trueGrit) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Matcher

newtype TrueGrit = TrueGrit AssetAttrs
  deriving anyclass (IsAsset, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

trueGrit :: AssetCard TrueGrit
trueGrit = assetWith TrueGrit Cards.trueGrit (healthL ?~ 3)

instance HasModifiersFor TrueGrit where
  getModifiersFor (TrueGrit a) = for_ a.controller \iid -> do
    modifySelect
      a
      (not_ (InvestigatorWithId iid) <> at_ (locationWithAsset a))
      [CanAssignDamageToAsset a.id]

instance RunMessage TrueGrit where
  runMessage msg (TrueGrit attrs) = TrueGrit <$> runMessage msg attrs
