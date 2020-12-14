{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.DrHenryArmitage
  ( DrHenryArmitage(..)
  , drHenryArmitage
  )
where

import Arkham.Import

import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner

newtype DrHenryArmitage = DrHenryArmitage Attrs
  deriving newtype (Show, ToJSON, FromJSON)

drHenryArmitage :: AssetId -> DrHenryArmitage
drHenryArmitage uuid = DrHenryArmitage $ (baseAttrs uuid "02040")
  { assetSlots = [AllySlot]
  , assetHealth = Just 2
  , assetSanity = Just 2
  }

fastAbility :: Attrs -> Window -> CardId -> Ability
fastAbility a window cid = (mkAbility (toSource a) 1 (FastAbility window))
  { abilityMetadata = Just (TargetMetadata $ CardIdTarget cid)
  }

instance HasModifiersFor env DrHenryArmitage where
  getModifiersFor = noModifiersFor

instance HasActions env DrHenryArmitage where
  getActions iid window@(AfterDrawCard You cid) (DrHenryArmitage a)
    | ownedBy a iid = pure
      [ ActivateCardAbilityAction iid (fastAbility a window cid)
      | not (assetExhausted a)
      ]
  getActions _ _ _ = pure []

instance (AssetRunner env) => RunMessage env DrHenryArmitage where
  runMessage msg (DrHenryArmitage attrs) = case msg of
    UseCardAbility iid source (Just (TargetMetadata (CardIdTarget cid))) 1
      | isSource attrs source -> do
        unshiftMessage
          $ chooseOne iid [DiscardCard iid cid, TakeResources iid 3 False]
        pure $ DrHenryArmitage $ attrs & exhaustedL .~ True
    _ -> DrHenryArmitage <$> runMessage msg attrs
