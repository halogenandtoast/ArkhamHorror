{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.MonstrousTransformation where

import Arkham.Import

import qualified Arkham.Types.Action as Action
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner

newtype MonstrousTransformation = MonstrousTransformation Attrs
  deriving newtype (Show, ToJSON, FromJSON)

monstrousTransformation :: AssetId -> MonstrousTransformation
monstrousTransformation uuid =
  MonstrousTransformation $ baseAttrs uuid "81030" $ pure ()

instance HasModifiersFor env MonstrousTransformation where
  getModifiersFor _ (InvestigatorTarget iid) (MonstrousTransformation a)
    | ownedBy a iid = pure
      [ BaseSkillOf SkillWillpower 2
      , BaseSkillOf SkillIntellect 2
      , BaseSkillOf SkillCombat 5
      , BaseSkillOf SkillAgility 5
      ]
  getModifiersFor _ _ _ = pure []

instance ActionRunner env => HasActions env MonstrousTransformation where
  getActions iid window (MonstrousTransformation a) | ownedBy a iid = do
    fightAvailable <- hasFightActions iid window
    pure
      [ ActivateCardAbilityAction
          iid
          (mkAbility (toSource a) 1 (ActionAbility 1 (Just Action.Fight)))
      | not (assetExhausted a) && fightAvailable
      ]
  getActions _ _ _ = pure []

instance (AssetRunner env) => RunMessage env MonstrousTransformation where
  runMessage msg (MonstrousTransformation attrs) = case msg of
    UseCardAbility iid source _ 1 | isSource attrs source -> do
      unshiftMessage
        $ ChooseFightEnemy iid source SkillCombat [DamageDealt 1] mempty False
      pure $ MonstrousTransformation $ attrs & exhausted .~ True
    _ -> MonstrousTransformation <$> runMessage msg attrs
