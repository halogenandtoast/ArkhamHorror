module Arkham.Types.Asset.Cards.JennysTwin45s
  ( JennysTwin45s(..)
  , jennysTwin45s
  )
where

import Arkham.Prelude

import Arkham.Types.Ability
import Arkham.Types.AssetId
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Effect.Window
import Arkham.Types.EffectMetadata
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.SkillType
import Arkham.Types.Slot
import Arkham.Types.Target
import qualified Arkham.Types.Action as Action
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner
import Arkham.Types.Asset.Uses

newtype JennysTwin45s = JennysTwin45s AssetAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

jennysTwin45s :: AssetId -> JennysTwin45s
jennysTwin45s uuid =
  JennysTwin45s $ (baseAttrs uuid "02010") { assetSlots = [HandSlot, HandSlot] }

instance HasModifiersFor env JennysTwin45s where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env JennysTwin45s where
  getActions iid window (JennysTwin45s a) | ownedBy a iid = do
    fightAvailable <- hasFightActions iid window
    pure
      [ ActivateCardAbilityAction
          iid
          (mkAbility
            (toSource a)
            1
            (ActionAbility
              (Just Action.Fight)
              (Costs [ActionCost 1, UseCost (toId a) Ammo 1])
            )
          )
      | fightAvailable
      ]
  getActions _ _ _ = pure []

instance AssetRunner env => RunMessage env JennysTwin45s where
  runMessage msg a@(JennysTwin45s attrs) = case msg of
    InvestigatorPlayDynamicAsset _ aid _ _ n | aid == assetId attrs ->
      JennysTwin45s <$> runMessage msg (attrs & usesL .~ Uses Ammo n)
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      a <$ unshiftMessages
        [ CreateWindowModifierEffect EffectSkillTestWindow
          (EffectModifiers
          $ toModifiers attrs [DamageDealt 1, SkillModifier SkillCombat 2]
          )
          source
          (InvestigatorTarget iid)
        , ChooseFightEnemy iid source SkillCombat False
        ]
    _ -> JennysTwin45s <$> runMessage msg attrs
