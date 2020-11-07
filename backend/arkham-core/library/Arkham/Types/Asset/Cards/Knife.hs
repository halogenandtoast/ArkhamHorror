{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.Knife where

import Arkham.Import

import qualified Arkham.Types.Action as Action
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner

newtype Knife = Knife Attrs
  deriving newtype (Show, ToJSON, FromJSON)

knife :: AssetId -> Knife
knife uuid = Knife $ baseAttrs uuid "01086" $ slots .= [HandSlot]

instance HasModifiersFor env Knife where
  getModifiersFor _ _ _ = pure []

instance ActionRunner env => HasActions env Knife where
  getActions iid NonFast (Knife a) | ownedBy a iid = do
    fightAvailable <- hasFightActions iid NonFast
    pure
      $ [ ActivateCardAbilityAction
            iid
            (mkAbility (toSource a) 1 (ActionAbility 1 (Just Action.Fight)))
        | fightAvailable
        ]
      <> [ ActivateCardAbilityAction
             iid
             (mkAbility (toSource a) 2 (ActionAbility 1 (Just Action.Fight)))
         | fightAvailable
         ]
  getActions _ _ _ = pure []

instance (AssetRunner env) => RunMessage env Knife where
  runMessage msg a@(Knife attrs) = case msg of
    UseCardAbility iid source _ 1 | isSource attrs source -> a <$ unshiftMessage
      (ChooseFightEnemy
        iid
        source
        SkillCombat
        [SkillModifier SkillCombat 1]
        mempty
        False
      )
    UseCardAbility iid source _ 2 | isSource attrs source ->
      a <$ unshiftMessages
        [ Discard (toTarget attrs)
        , ChooseFightEnemy
          iid
          source
          SkillCombat
          [SkillModifier SkillCombat 2, DamageDealt 1]
          mempty
          False
        ]
    _ -> Knife <$> runMessage msg attrs
