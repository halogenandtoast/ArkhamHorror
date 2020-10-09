{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.Machete where

import Arkham.Import

import qualified Arkham.Types.Action as Action
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner

newtype Machete = Machete Attrs
  deriving newtype (Show, ToJSON, FromJSON)

machete :: AssetId -> Machete
machete uuid = Machete $ (baseAttrs uuid "01020") { assetSlots = [HandSlot] }

instance HasModifiersFor env investigator Machete where
  getModifiersFor _ _ _ = pure []

instance (ActionRunner env investigator) => HasActions env investigator Machete where
  getActions i window (Machete a) | ownedBy a i = do
    fightAvailable <- hasFightActions i window
    pure
      [ ActivateCardAbilityAction
          (getId () i)
          (mkAbility (toSource a) 1 (ActionAbility 1 (Just Action.Fight)))
      | fightAvailable
      ]
  getActions _ _ _ = pure []

instance (AssetRunner env) => RunMessage env Machete where
  runMessage msg a@(Machete attrs@Attrs {..}) = case msg of
    UseCardAbility iid source _ 1 | isSource attrs source -> do
      criteriaMet <- asks $ (== 1) . unEnemyCount . getCount iid
      a <$ unshiftMessage
        (ChooseFightEnemy
          iid
          source
          SkillCombat
          ([ DamageDealt 1 | criteriaMet ] <> [SkillModifier SkillCombat 1])
          mempty
          False
        )
    _ -> Machete <$> runMessage msg attrs
