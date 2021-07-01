module Arkham.Types.Asset.Cards.Shotgun4
  ( Shotgun4(..)
  , shotgun4
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import qualified Arkham.Types.Action as Action
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner
import Arkham.Types.Asset.Uses
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Effect.Window
import Arkham.Types.EffectMetadata
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.SkillType
import Arkham.Types.Slot
import Arkham.Types.Target

newtype Shotgun4 = Shotgun4 AssetAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shotgun4 :: AssetCard Shotgun4
shotgun4 = assetWith Shotgun4 Cards.shotgun4 (slotsL .~ [HandSlot, HandSlot])

instance HasModifiersFor env Shotgun4 where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env Shotgun4 where
  getActions iid window (Shotgun4 a) | ownedBy a iid = do
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

instance (AssetRunner env) => RunMessage env Shotgun4 where
  runMessage msg a@(Shotgun4 attrs) = case msg of
    InvestigatorPlayAsset _ aid _ _ | aid == assetId attrs ->
      Shotgun4 <$> runMessage msg (attrs & usesL .~ Uses Ammo 2)
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      a <$ unshiftMessages
        [ CreateWindowModifierEffect
          EffectSkillTestWindow
          (EffectModifiers $ toModifiers attrs [SkillModifier SkillCombat 3])
          source
          (InvestigatorTarget iid)
        , ChooseFightEnemy iid source SkillCombat mempty False
        ]
    FailedSkillTest iid _ source SkillTestInitiatorTarget{} _ n
      | isSource attrs source
      -> let val = min 1 (max 5 n)
         in
           a <$ unshiftMessage
             (CreateWindowModifierEffect
               EffectSkillTestWindow
               (EffectModifiers $ toModifiers attrs [DamageDealt val])
               source
               (InvestigatorTarget iid)
             )
    PassedSkillTest iid _ source SkillTestInitiatorTarget{} _ n
      | isSource attrs source
      -> let val = min 1 (max 5 n)
         in
           a <$ unshiftMessage
             (CreateWindowModifierEffect
               EffectSkillTestWindow
               (EffectModifiers $ toModifiers attrs [DamageDealt val])
               source
               (InvestigatorTarget iid)
             )
    _ -> Shotgun4 <$> runMessage msg attrs
