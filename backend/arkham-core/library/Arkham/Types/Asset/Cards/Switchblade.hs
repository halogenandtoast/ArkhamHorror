{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.Switchblade where

import Arkham.Import

import qualified Arkham.Types.Action as Action
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner

newtype Switchblade = Switchblade Attrs
  deriving newtype (Show, ToJSON, FromJSON)

switchblade :: AssetId -> Switchblade
switchblade uuid = Switchblade $ baseAttrs uuid "01044" $ slots .= [HandSlot]

instance HasModifiersFor env Switchblade where
  getModifiersFor _ _ _ = pure []

instance ActionRunner env => HasActions env Switchblade where
  getActions iid window (Switchblade a) | ownedBy a iid = do
    let
      ability = mkAbility (toSource a) 1 (ActionAbility 1 (Just Action.Fight))
    fightAvailable <- hasFightActions iid window
    pure [ ActivateCardAbilityAction iid ability | fightAvailable ]
  getActions _ _ _ = pure []

instance (AssetRunner env) => RunMessage env Switchblade where
  runMessage msg a@(Switchblade attrs) = case msg of
    UseCardAbility iid source _ 1 | isSource attrs source -> a <$ unshiftMessage
      (ChooseFightEnemy
        iid
        source
        SkillCombat
        [ModifierIfSucceededBy 2 (DamageDealt 1)]
        mempty
        False
      )
    _ -> Switchblade <$> runMessage msg attrs
