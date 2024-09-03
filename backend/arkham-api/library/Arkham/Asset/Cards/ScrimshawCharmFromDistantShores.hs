module Arkham.Asset.Cards.ScrimshawCharmFromDistantShores (
  scrimshawCharmFromDistantShores,
  ScrimshawCharmFromDistantShores (..),
)
where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype ScrimshawCharmFromDistantShores = ScrimshawCharmFromDistantShores AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

scrimshawCharmFromDistantShores :: AssetCard ScrimshawCharmFromDistantShores
scrimshawCharmFromDistantShores = asset ScrimshawCharmFromDistantShores Cards.scrimshawCharmFromDistantShores

instance HasAbilities ScrimshawCharmFromDistantShores where
  getAbilities (ScrimshawCharmFromDistantShores x) =
    [ restrictedAbility x 1 ControlsThis
        $ ActionAbility []
        $ ActionCost 1
        <> exhaust x
        <> AddCurseTokensCost 1 3
    ]

instance RunMessage ScrimshawCharmFromDistantShores where
  runMessage msg a@(ScrimshawCharmFromDistantShores attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ (addedCurseTokenPayment -> n) -> do
      gainResourcesIfCan iid (attrs.ability 1) (1 + n)
      pure a
    _ -> ScrimshawCharmFromDistantShores <$> liftRunMessage msg attrs
