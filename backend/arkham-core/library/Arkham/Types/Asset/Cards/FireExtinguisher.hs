module Arkham.Types.Asset.Cards.FireExtinguisher where

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

newtype FireExtinguisher = FireExtinguisher AssetAttrs
  deriving newtype (Show, Eq, Generic, ToJSON, FromJSON, Entity)

fireExtinguisher :: AssetCard FireExtinguisher
fireExtinguisher = hand FireExtinguisher Cards.fireExtinguisher

instance HasModifiersFor env FireExtinguisher where
  getModifiersFor = noModifiersFor

instance HasActions env ActionType => HasActions env FireExtinguisher where
  getActions iid window (FireExtinguisher a) | ownedBy a iid = do
    fightAvailable <- hasFightActions iid window
    evadeAvailable <- hasEvadeActions iid window
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
               (ActionAbility (Just Action.Evade) (Costs [ActionCost 1, ExileCost $ toTarget a]))
             )
         | evadeAvailable
         ]
  getActions i window (FireExtinguisher x) = getActions i window x

instance (AssetRunner env) => RunMessage env FireExtinguisher where
  runMessage msg a@(FireExtinguisher attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      a <$ unshiftMessages
        [ CreateWindowModifierEffect
          EffectSkillTestWindow
          (EffectModifiers $ toModifiers attrs [SkillModifier SkillCombat 1])
          source
          (InvestigatorTarget iid)
        , ChooseFightEnemy iid source SkillCombat mempty False
        ]
    _ -> FireExtinguisher <$> runMessage msg attrs
