{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.DrFrancisMorgan
  ( drFrancisMorgan
  , DrFrancisMorgan(..)
  )
where

import Arkham.Import

import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers

newtype DrFrancisMorgan = DrFrancisMorgan Attrs
  deriving newtype (Show, ToJSON, FromJSON)

drFrancisMorgan :: AssetId -> DrFrancisMorgan
drFrancisMorgan uuid = DrFrancisMorgan $ (baseAttrs uuid "02080")
  { assetSlots = [AllySlot]
  , assetHealth = Just 4
  , assetSanity = Just 1
  }

ability :: Attrs -> Ability
ability attrs =
  mkAbility (toSource attrs) 1 (ReactionAbility (AfterEnemyDefeated You))

instance HasActions env DrFrancisMorgan where
  getActions iid (AfterEnemyDefeated You) (DrFrancisMorgan attrs)
    | ownedBy attrs iid = pure
      [ ActivateCardAbilityAction iid (ability attrs)
      | not (assetExhausted attrs)
      ]
  getActions iid window (DrFrancisMorgan attrs) = getActions iid window attrs

instance HasModifiersFor env DrFrancisMorgan where
  getModifiersFor _ (InvestigatorTarget iid) (DrFrancisMorgan a) =
    pure [ toModifier a (SkillModifier SkillCombat 1) | ownedBy a iid ]
  getModifiersFor _ _ _ = pure []

instance (HasQueue env, HasModifiersFor env ()) => RunMessage env DrFrancisMorgan where
  runMessage msg (DrFrancisMorgan attrs) = case msg of
    UseCardAbility iid source _ 1 | isSource attrs source -> do
      unshiftMessage (DrawCards iid 1 False)
      pure $ DrFrancisMorgan $ attrs & exhaustedL .~ True
    _ -> DrFrancisMorgan <$> runMessage msg attrs
