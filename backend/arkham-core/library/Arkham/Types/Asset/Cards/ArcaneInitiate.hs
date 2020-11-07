{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.ArcaneInitiate where

import Arkham.Import

import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.Trait

newtype ArcaneInitiate = ArcaneInitiate Attrs
  deriving newtype (Show, ToJSON, FromJSON)

arcaneInitiate :: AssetId -> ArcaneInitiate
arcaneInitiate uuid = ArcaneInitiate $ baseAttrs uuid "01063" $ do
  slots .= [AllySlot]
  health ?= 1
  sanity ?= 2

fastAbility :: Attrs -> Window -> Ability
fastAbility a window = mkAbility (toSource a) 1 (FastAbility window)

instance HasModifiersFor env ArcaneInitiate where
  getModifiersFor _ _ _ = pure []

instance HasActions env ArcaneInitiate where
  getActions iid window (ArcaneInitiate a) | ownedBy a iid = pure
    [ ActivateCardAbilityAction iid (fastAbility a window)
    | not (assetExhausted a)
    ]
  getActions _ _ _ = pure []

instance (AssetRunner env) => RunMessage env ArcaneInitiate where
  runMessage msg (ArcaneInitiate attrs) = case msg of
    InvestigatorPlayAsset _ aid _ _ | aid == assetId attrs ->
      ArcaneInitiate <$> runMessage msg (attrs & doom +~ 1)
    UseCardAbility iid source _ 1 | isSource attrs source -> do
      unshiftMessage $ chooseOne
        iid
        [SearchTopOfDeck iid (InvestigatorTarget iid) 3 [Spell] ShuffleBackIn]
      pure $ ArcaneInitiate $ attrs & exhausted .~ True
    _ -> ArcaneInitiate <$> runMessage msg attrs
