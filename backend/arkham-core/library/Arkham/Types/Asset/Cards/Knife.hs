module Arkham.Types.Asset.Cards.Knife
  ( Knife(..)
  , knife
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import qualified Arkham.Types.Action as Action
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Effect.Window
import Arkham.Types.EffectMetadata
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.SkillType
import Arkham.Types.Target
import Arkham.Types.Window

newtype Knife = Knife AssetAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

knife :: AssetCard Knife
knife = hand Knife Cards.knife

instance HasModifiersFor env Knife where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env Knife where
  getActions iid NonFast (Knife a) | ownedBy a iid = do
    fightAvailable <- hasFightActions iid NonFast
    pure
      $ [ ActivateCardAbilityAction
            iid
            (mkAbility
              (toSource a)
              1
              (ActionAbility (Just Action.Fight) (ActionCost 1))
            )
        | fightAvailable
        ]
      <> [ ActivateCardAbilityAction
             iid
             (mkAbility
               (toSource a)
               2
               (ActionAbility (Just Action.Fight) (ActionCost 1))
             )
         | fightAvailable
         ]
  getActions _ _ _ = pure []

instance (AssetRunner env) => RunMessage env Knife where
  runMessage msg a@(Knife attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      a <$ unshiftMessages
        [ CreateWindowModifierEffect
          EffectSkillTestWindow
          (EffectModifiers $ toModifiers attrs [SkillModifier SkillCombat 1])
          source
          (InvestigatorTarget iid)
        , ChooseFightEnemy iid source SkillCombat mempty False
        ]
    UseCardAbility iid source _ 2 _ | isSource attrs source ->
      a <$ unshiftMessages
        [ Discard (toTarget attrs)
        , CreateWindowModifierEffect
          EffectSkillTestWindow
          (EffectModifiers
          $ toModifiers attrs [SkillModifier SkillCombat 2, DamageDealt 1]
          )
          source
          (InvestigatorTarget iid)
        , ChooseFightEnemy iid source SkillCombat mempty False
        ]
    _ -> Knife <$> runMessage msg attrs
