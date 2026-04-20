module Arkham.Asset.Assets.CornHuskDoll (cornHuskDoll) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (RevealChaosToken)
import Arkham.Asset.Uses
import Arkham.Helpers.Window (getRevealedChaosTokens)
import Arkham.Matcher
import Arkham.Modifier

newtype CornHuskDoll = CornHuskDoll AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cornHuskDoll :: AssetCard CornHuskDoll
cornHuskDoll = assetWith CornHuskDoll Cards.cornHuskDoll (healthL ?~ 1)

instance HasAbilities CornHuskDoll where
  getAbilities (CornHuskDoll a) =
    [ restricted a 1 OnSameLocation
        $ triggered
          (RevealChaosToken #when (affectsColocatedMatch You) (oneOf [#skull, #tablet]))
          (assetUseCost a Wish 1 <> exhaust a)
    ]

instance RunMessage CornHuskDoll where
  runMessage msg a@(CornHuskDoll attrs) = runQueueT $ case msg of
    UseCardAbility _iid (isSource attrs -> True) 1 (getRevealedChaosTokens -> [token]) _ -> do
      chaosTokenEffect (attrs.ability 1) token $ ChaosTokenFaceModifier [#cultist]
      pure a
    _ -> CornHuskDoll <$> liftRunMessage msg attrs
