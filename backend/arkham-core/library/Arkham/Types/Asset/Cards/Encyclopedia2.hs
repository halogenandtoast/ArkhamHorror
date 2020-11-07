{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.Encyclopedia2 where

import Arkham.Import

import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner

newtype Encyclopedia2 = Encyclopedia2 Attrs
  deriving newtype (Show, ToJSON, FromJSON)

encyclopedia2 :: AssetId -> Encyclopedia2
encyclopedia2 uuid =
  Encyclopedia2 $ baseAttrs uuid "01042" $ slots .= [HandSlot]

instance HasModifiersFor env Encyclopedia2 where
  getModifiersFor _ _ _ = pure []

instance HasActions env Encyclopedia2 where
  getActions iid NonFast (Encyclopedia2 a) | ownedBy a iid = pure
    [ ActivateCardAbilityAction
        iid
        (mkAbility (toSource a) 1 (ActionAbility 1 Nothing))
    | not (assetExhausted a)
    ]
  getActions _ _ _ = pure []

instance (AssetRunner env) => RunMessage env Encyclopedia2 where
  runMessage msg a@(Encyclopedia2 attrs) = case msg of
    EndPhase -> a <$ unshiftMessage (RemoveAllModifiersFrom (toSource attrs))
    UseCardAbility iid source _ 1 | isSource attrs source -> do
      locationId <- asks $ getId @LocationId iid
      investigatorTargets <-
        asks $ map InvestigatorTarget . setToList . getSet locationId
      unshiftMessage $ chooseOne
        iid
        [ TargetLabel
            target
            [ chooseOne
                iid
                [ Label
                  "Willpower"
                  [AddModifiers target source [SkillModifier SkillWillpower 2]]
                , Label
                  "Intellect"
                  [AddModifiers target source [SkillModifier SkillIntellect 2]]
                , Label
                  "Combat"
                  [AddModifiers target source [SkillModifier SkillCombat 2]]
                , Label
                  "Agility"
                  [AddModifiers target source [SkillModifier SkillAgility 2]]
                ]
            ]
        | target <- investigatorTargets
        ]
      pure $ Encyclopedia2 $ attrs & exhausted .~ True
    _ -> Encyclopedia2 <$> runMessage msg attrs
