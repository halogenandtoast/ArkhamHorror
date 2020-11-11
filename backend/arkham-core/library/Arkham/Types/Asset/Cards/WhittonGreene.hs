{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.WhittonGreene
  ( whittonGreene
  , WhittonGreene(..)
  )
where

import Arkham.Import

import Arkham.Types.Asset.Attrs
import Arkham.Types.Trait

newtype WhittonGreene = WhittonGreene Attrs
  deriving newtype (Show, ToJSON, FromJSON)

whittonGreene :: AssetId -> WhittonGreene
whittonGreene uuid = WhittonGreene $ (baseAttrs uuid "60213")
  { assetSlots = [AllySlot]
  , assetHealth = Just 2
  , assetSanity = Just 2
  }

instance HasActions env WhittonGreene where
  getActions iid window@(AfterRevealLocation You) (WhittonGreene a)
    | ownedBy a iid = do
      let ability = mkAbility (toSource a) 1 (ReactionAbility window)
      pure [ ActivateCardAbilityAction iid ability | not (assetExhausted a) ]
  getActions iid window@(AfterPutLocationIntoPlay You) (WhittonGreene a)
    | ownedBy a iid = do
      let ability = mkAbility (toSource a) 1 (ReactionAbility window)
      pure [ ActivateCardAbilityAction iid ability | not (assetExhausted a) ]
  getActions iid window (WhittonGreene attrs) = getActions iid window attrs

instance HasCount AssetCount (InvestigatorId, [Trait]) env => HasModifiersFor env WhittonGreene where
  getModifiersFor _ (InvestigatorTarget iid) (WhittonGreene a) | ownedBy a iid =
    do
      active <- asks $ (> 0) . unAssetCount . getCount (iid, [Tome, Relic])
      pure [ SkillModifier SkillIntellect 1 | active ]
  getModifiersFor _ _ _ = pure []

instance (HasQueue env, HasModifiers env InvestigatorId) => RunMessage env WhittonGreene where
  runMessage msg (WhittonGreene attrs@Attrs {..}) = case msg of
    UseCardAbility iid source _ 1 | isSource attrs source -> do
      unshiftMessage
        (SearchTopOfDeck
          iid
          (InvestigatorTarget iid)
          6
          [Tome, Relic]
          ShuffleBackIn
        )
      pure . WhittonGreene $ attrs & exhausted .~ True
    _ -> WhittonGreene <$> runMessage msg attrs
