module Arkham.Asset.Assets.WavewornIdol (wavewornIdol) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Matcher
import Arkham.Trait (Trait (DeepOne))

newtype WavewornIdol = WavewornIdol AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wavewornIdol :: AssetCard WavewornIdol
wavewornIdol = assetWith WavewornIdol Cards.wavewornIdol (sanityL ?~ 2)

instance HasAbilities WavewornIdol where
  getAbilities (WavewornIdol x) =
    [ controlled_ x 1
        $ triggered
          (oneOf [FloodLevelChanged #after YourLocation, EnemySpawns #after YourLocation (withTrait DeepOne)])
          (exhaust x)
    ]

instance RunMessage WavewornIdol where
  runMessage msg a@(WavewornIdol attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      takeActionAsIfTurn iid (attrs.ability 1)
      pure a
    _ -> WavewornIdol <$> liftRunMessage msg attrs
