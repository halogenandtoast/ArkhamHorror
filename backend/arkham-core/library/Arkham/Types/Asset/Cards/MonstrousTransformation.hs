module Arkham.Types.Asset.Cards.MonstrousTransformation
  ( MonstrousTransformation(..)
  , monstrousTransformation
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
import Arkham.Types.Target
import qualified Arkham.Types.Action as Action
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner

newtype MonstrousTransformation = MonstrousTransformation AssetAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

monstrousTransformation :: AssetId -> MonstrousTransformation
monstrousTransformation uuid =
  MonstrousTransformation $ (baseAttrs uuid "81030") { assetIsStory = True }

instance HasModifiersFor env MonstrousTransformation where
  getModifiersFor _ (InvestigatorTarget iid) (MonstrousTransformation a)
    | ownedBy a iid = pure $ toModifiers
      a
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
          (mkAbility
            (toSource a)
            1
            (ActionAbility (Just Action.Fight) (ActionCost 1))
          )
      | not (assetExhausted a) && fightAvailable
      ]
  getActions _ _ _ = pure []

instance (AssetRunner env) => RunMessage env MonstrousTransformation where
  runMessage msg (MonstrousTransformation attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      unshiftMessages
        [ CreateWindowModifierEffect EffectSkillTestWindow
          (EffectModifiers $ toModifiers attrs [DamageDealt 1])
          source
          (InvestigatorTarget iid)
        , ChooseFightEnemy iid source SkillCombat False
        ]
      pure $ MonstrousTransformation $ attrs & exhaustedL .~ True
    _ -> MonstrousTransformation <$> runMessage msg attrs
