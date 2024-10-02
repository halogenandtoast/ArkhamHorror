module Arkham.Asset.Assets.WavewornIdol (wavewornIdol, WavewornIdol (..)) where

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
    [ restricted x 1 ControlsThis
        $ ReactionAbility
          (oneOf [FloodLevelChanged #after YourLocation, EnemySpawns #after YourLocation (withTrait DeepOne)])
          (exhaust x)
    ]

instance RunMessage WavewornIdol where
  runMessage msg a@(WavewornIdol attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      pushAll [GainActions iid (toSource attrs) 1, PlayerWindow iid [] False]
      pure a
    _ -> WavewornIdol <$> liftRunMessage msg attrs
