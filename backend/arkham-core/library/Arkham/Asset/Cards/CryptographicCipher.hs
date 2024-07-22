module Arkham.Asset.Cards.CryptographicCipher (
  cryptographicCipher,
  CryptographicCipher (..),
)
where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Helpers.Investigator
import Arkham.Investigate
import Arkham.Prelude

newtype CryptographicCipher = CryptographicCipher AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cryptographicCipher :: AssetCard CryptographicCipher
cryptographicCipher =
  asset CryptographicCipher Cards.cryptographicCipher

instance HasAbilities CryptographicCipher where
  getAbilities (CryptographicCipher x) =
    [ withTooltip
        "Exhaust Cryptographic Cipher and spend 1 secret: Investigate. Your location gets +1 shroud for this investigation."
        $ restrictedAbility x 1 ControlsThis
        $ FastAbility' (assetUseCost x Supply 1 <> exhaust x) [#investigate]
    , withTooltip
        "Exhaust Cryptographic Cipher and spend 1 secret: Investigate. Your location gets -2 shroud for this investigation."
        $ investigateAbility x 1 (assetUseCost x Supply 1 <> exhaust x) ControlsThis
    ]

instance RunMessage CryptographicCipher where
  runMessage msg a@(CryptographicCipher attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      lid <- getJustLocation iid
      sid <- getRandom
      investigation <- mkInvestigate sid iid (toAbilitySource attrs 1)
      pushAll
        [ skillTestModifier sid attrs lid (ShroudModifier 1)
        , toMessage investigation
        ]
      pure a
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      lid <- getJustLocation iid
      sid <- getRandom
      investigation <- mkInvestigate sid iid (toAbilitySource attrs 1)
      pushAll
        [ skillTestModifier sid attrs lid (ShroudModifier (-2))
        , toMessage investigation
        ]
      pure a
    _ -> CryptographicCipher <$> runMessage msg attrs
